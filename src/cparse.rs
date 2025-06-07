use crate::types::{
  ctype_from_string, is_primitive_ctype, CType, ISLBorrowRule, ISLEnum, ISLFunction, Parameter,
};
use anyhow::{bail, Result};
use clang_ast::BareSourceLocation;
use lazy_static::lazy_static;
use serde::Serialize;
use serde_derive::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;
use std::process::Command;

lazy_static! {
  static ref UNSUPPORTED_FUNCS: HashSet<&'static str> =
    HashSet::from(["isl_space_extend",
                   "isl_basic_set_has_defining_equality",
                   "isl_basic_set_has_defining_inequalities"]);
}

type Node = clang_ast::Node<Clang>;

#[derive(Serialize, Deserialize, Debug, Clone)]
enum Clang {
  TranslationUnitDecl(TranslationUnitDecl),
  FunctionDecl(FunctionDecl),
  ParmVarDecl(ParmVarDecl),
  EnumDecl(EnumDecl),
  EnumConstantDecl(EnumConstantDecl),
  DeprecatedAttr(DeprecatedAttr),
  Unknown(UnknownNode),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct TranslationUnitDecl {
  pub loc: Option<clang_ast::SourceLocation>,
  pub range: Option<clang_ast::SourceRange>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct FunctionDecl {
  pub name: String,
  #[serde(rename = "type")]
  pub type_: Type,
  pub loc: Option<clang_ast::SourceLocation>,
  pub range: Option<clang_ast::SourceRange>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct EnumDecl {
  pub name: Option<String>,
  pub loc: Option<clang_ast::SourceLocation>,
  pub range: Option<clang_ast::SourceRange>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct EnumConstantDecl {
  pub name: String,
  pub loc: Option<clang_ast::SourceLocation>,
  pub range: Option<clang_ast::SourceRange>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct ParmVarDecl {
  pub name: Option<String>,
  #[serde(rename = "type")]
  pub type_: Type,
  pub loc: Option<clang_ast::SourceLocation>,
  pub range: Option<clang_ast::SourceRange>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct DeprecatedAttr {
  pub loc: Option<clang_ast::SourceLocation>,
  pub range: Option<clang_ast::SourceRange>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct UnknownNode {
  pub kind: String,
  pub loc: Option<clang_ast::SourceLocation>,
  pub range: Option<clang_ast::SourceRange>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Type {
  #[serde(rename = "qualType")]
  pub qual_type: String,
}

#[derive(Debug, Clone)]
struct LocTriple {
  pub file: String,
  pub line: usize,
  pub col: usize,
}

pub struct ParseState {
  pub file_to_string: HashMap<String, Vec<String>>,
}

fn cfile_to_json(file: &String) -> Result<String> {
  // Run clang command
  let output = Command::new("clang").args(["-I",
                                           "isl/include/",
                                           "-I",
                                           "/usr/lib/llvm-19/lib/clang/19/include/",
                                           "-Xclang",
                                           "-ast-dump=json",
                                           "-Xclang",
                                           "-detailed-preprocessing-record",
                                           file.as_str()])
                                    .output()?;

  // Check if command was successful
  if !output.status.success() {
    anyhow::bail!("clang failed with status {}: {}",
                  output.status,
                  String::from_utf8_lossy(&output.stderr));
  }
  let json_content = String::from_utf8(output.stdout)?;

  // println!("JSON={}", json_content);

  return Ok(json_content);
}

fn get_loc_triple(src_loc: &BareSourceLocation) -> LocTriple {
  LocTriple { file: src_loc.file.to_string(),
              line: src_loc.line,
              col: src_loc.col }
}

fn get_line_in_file(file: &String, line: usize, state: &mut ParseState) -> String {
  if !state.file_to_string.contains_key(file) {
    let lines_in_file = read_to_string(file).unwrap()
                                            .lines()
                                            .map(String::from)
                                            .collect();
    state.file_to_string.insert(file.clone(), lines_in_file);
  }
  let lines_in_file = state.file_to_string.get(file).unwrap();
  return lines_in_file[line - 1].clone();
}

fn get_borrow_rule_between(loc1_: &LocTriple, loc2: &LocTriple, state: &mut ParseState)
                           -> Result<ISLBorrowRule> {
  let loc1 = if loc1_.file == "<start_line_of_loc2>" {
    LocTriple { file: loc2.file.clone(),
                col: 1,
                line: loc2.line }
  } else {
    loc1_.clone()
  };

  assert!(loc1.file == loc2.file,
          "Loc1 in {:#?}. Loc2 in {:#?}",
          loc1.file,
          loc2.file);
  let file = &loc1.file;

  let mut lines: Vec<String> = vec![];
  for line in loc1.line..loc2.line + 1 {
    if line == loc1.line && line == loc2.line {
      lines.push(get_line_in_file(&file, loc2.line, state)[loc1.col - 1..loc2.col].to_string());
    } else if line == loc1.line {
      lines.push(get_line_in_file(&file, line, state)[loc1.col - 1..].to_string());
    } else if line == loc2.line {
      lines.push(get_line_in_file(&file, line, state)[..loc2.col].to_string());
    } else {
      lines.push(get_line_in_file(&file, line, state));
    }
  }

  let in_between_code = lines.join("\n");
  if in_between_code.contains("__isl_keep") {
    Ok(ISLBorrowRule::IslKeep)
  } else if in_between_code.contains("__isl_take") {
    Ok(ISLBorrowRule::IslTake)
  } else {
    bail!("Could not infer the borrow rule from {}", in_between_code);
  }
}

fn get_function_from_decl(func_decl: &FunctionDecl, inner: &Vec<Node>, state: &mut ParseState)
                          -> Result<ISLFunction> {
  let mut func_params: Vec<Parameter> = vec![];

  let mut prev_loc = LocTriple { file: "<start_line_of_loc2>".to_string(),
                                 line: 0,
                                 col: 0 };
  // print!("Function {} at ({}, {}, {}). Takes: ",
  //        func_decl.name, prev_loc.file, prev_loc.line, prev_loc.col);

  for (_iparam, func_decl_inner) in inner.iter().enumerate() {
    match &func_decl_inner.kind {
      Clang::ParmVarDecl(param_decl) => {
        let param_type = ctype_from_string(&param_decl.type_.qual_type.clone())?;
        let param_end_loc = get_loc_triple(&param_decl.range
                                                      .clone()
                                                      .unwrap()
                                                      .end
                                                      .clone()
                                                      .spelling_loc
                                                      .unwrap());
        let borrow_rule = if param_type == CType::ISLCtx {
          ISLBorrowRule::IslKeep
        } else if param_type == CType::ISLArgs {
          ISLBorrowRule::IslKeep
        } else if is_primitive_ctype(param_type) {
          ISLBorrowRule::PassByValue
        } else if param_type == CType::Unsupported {
          ISLBorrowRule::Unsupported
        } else if param_type == CType::CString {
          // FIXME: Make sure that this is correct.
          ISLBorrowRule::IslKeep
        } else if func_decl.name.ends_with("_copy") {
          ISLBorrowRule::IslKeep
        } else if func_decl.name.ends_with("_free") {
          ISLBorrowRule::IslTake
        } else {
          get_borrow_rule_between(&prev_loc, &param_end_loc, state)?
        };
        // print!("{}[{}, {}, {}, {}], ",
        //        param_decl.name.clone().unwrap(),
        //        param_end_loc.file,
        //        param_end_loc.line,
        //        param_end_loc.col,
        //        borrow_rule);

        func_params.push(Parameter { name: param_decl.name.clone().unwrap(),
                                     type_: param_type,
                                     borrow: borrow_rule });
        prev_loc = param_end_loc;
      }
      Clang::DeprecatedAttr(_) => { /*Do nothing for attribute */ }
      _ => bail!("Expect a func decl's inner to be a param, got {:#?}.",
                 func_decl_inner),
    }
  }
  // println!(".");
  // println!("Parsing the ret-type from {}",
  //          &func_decl.type_
  //                    .qual_type
  //                    .clone()
  //                    .split("(")
  //                    .next()
  //                    .unwrap()
  //                    .to_string());
  let ret_type = ctype_from_string(&func_decl.type_
                                             .qual_type
                                             .clone()
                                             .split("(")
                                             .next()
                                             .unwrap()
                                             .to_string())?;

  // FIXME: Get the return type!!
  return Ok(ISLFunction { name: func_decl.name.clone(),
                          parameters: func_params,
                          ret_type: ret_type });
}

fn get_enum_from_decl(enum_decl: &EnumDecl, inner: &Vec<Node>) -> Result<ISLEnum> {
  let mut variants: Vec<String> = vec![];
  for enum_decl_inner in inner {
    match &enum_decl_inner.kind {
      Clang::EnumConstantDecl(enum_const_decl) => {
        variants.push(enum_const_decl.name.clone());
      }
      _ => bail!("Expect a enum decl's inner to be a enum-constant-decl."),
    }
  }
  // println!("Enum: {}",
  //          ISLEnum { name: enum_decl.name.clone().unwrap(),
  //                    variants: variants.clone() });
  return Ok(ISLEnum { name: enum_decl.name.clone().unwrap(),
                      variants: variants });
}

pub fn extract_functions(filename: &String, state: &mut ParseState) -> Result<Vec<ISLFunction>> {
  let ast_json = cfile_to_json(filename)?;
  let t_unit: Node = serde_json::from_str(&ast_json.as_str())?;
  // println!("node={:#?}", t_unit);

  let t_unit_body: Result<Vec<Node>> = match t_unit.kind {
    Clang::TranslationUnitDecl(_) => Ok(t_unit.inner),
    _ => bail!("Parsed file not a translation unit?"),
  };

  let mut isl_functions: Vec<ISLFunction> = vec![];

  for decl in t_unit_body? {
    match decl.kind {
      Clang::FunctionDecl(func_decl) => {
        let spelling_filename = func_decl.range
                                         .clone()
                                         .unwrap()
                                         .end
                                         .spelling_loc
                                         .unwrap()
                                         .file
                                         .to_string();
        // println!("Parsing {} of {}", func_decl.name, spelling_filename);
        if func_decl.name.starts_with("isl_")
           && !UNSUPPORTED_FUNCS.contains(func_decl.name.as_str())
        {
          if spelling_filename.starts_with("isl/include/") {
            // println!("Processing func {} in file {}.",
            //          func_decl.name, spelling_filename);
            isl_functions.push(get_function_from_decl(&func_decl, &decl.inner, state)?);
          }
        }
      }
      _ => {}
    }
  }
  return Ok(isl_functions);
}

pub fn extract_enums(filename: &String) -> Result<Vec<ISLEnum>> {
  let ast_json = cfile_to_json(filename)?;
  let t_unit: Node = serde_json::from_str(&ast_json.as_str())?;
  // println!("node={:#?}", t_unit);

  let t_unit_body: Result<Vec<Node>> = match t_unit.kind {
    Clang::TranslationUnitDecl(_) => Ok(t_unit.inner),
    _ => bail!("Parsed file not a translation unit?"),
  };

  let mut isl_enums: Vec<ISLEnum> = vec![];

  for decl in t_unit_body? {
    match decl.kind {
      Clang::EnumDecl(enum_decl) => match enum_decl.name.clone() {
        Some(name) => {
          if name.starts_with("isl_") {
            let spelling_filename = enum_decl.range
                                             .clone()
                                             .unwrap()
                                             .begin
                                             .spelling_loc
                                             .unwrap()
                                             .file
                                             .to_string();
            if spelling_filename.starts_with("isl/include/") {
              isl_enums.push(get_enum_from_decl(&enum_decl, &decl.inner)?)
            }
          }
        }
        _ => {}
      },
      _ => {}
    }
  }
  return Ok(isl_enums);
}
