use crate::types::{ctype_from_string, CType, ISLBorrowRule, ISLFunction, Parameter};
use anyhow::{bail, Result};
use clang_ast::BareSourceLocation;
use serde::Serialize;
use serde_derive::Deserialize;
use std::collections::HashMap;
use std::process::Command;

type Node = clang_ast::Node<Clang>;

#[derive(Serialize, Deserialize, Debug, Clone)]
enum Clang {
  TranslationUnitDecl(TranslationUnitDecl),
  FunctionDecl(FunctionDecl),
  ParmVarDecl(ParmVarDecl),
  EnumDecl(EnumDecl),
  EnumConstantDecl(EnumConstantDecl),
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

pub struct ParseState {
  pub file_to_string: HashMap<String, String>,
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

fn get_loc_triple(src_loc: &BareSourceLocation) -> (String, usize, usize) {
  (src_loc.file.to_string(), src_loc.line, src_loc.col)
}

fn get_function_from_decl(func_decl: &FunctionDecl, inner: &Vec<Node>, input_file: &String,
                          state: &mut ParseState)
                          -> Result<ISLFunction> {
  let mut func_params: Vec<Parameter> = vec![];

  let func_begin_loc = &func_decl.range.clone().unwrap().begin.spelling_loc.unwrap();
  let (func_file, func_line, func_col) = get_loc_triple(func_begin_loc);
  print!("Function {} at ({}, {}, {}). Takes: ",
         func_decl.name, func_file, func_line, func_col);

  for (iparam, func_decl_inner) in inner.iter().enumerate() {
    match &func_decl_inner.kind {
      Clang::ParmVarDecl(param_decl) => {
        let param_type = ctype_from_string(&param_decl.type_.qual_type.clone())?;
        let param_loc = &param_decl.loc.clone().unwrap().spelling_loc.unwrap();
        let (param_file, param_line, param_col) = get_loc_triple(param_loc);
        print!("{}[{}, {}, {}], ",
               param_decl.name.clone().unwrap(),
               param_file,
               param_line,
               param_col);

        // FIXME: Care about borrowship rules, type
        func_params.push(Parameter { name: param_decl.name.clone().unwrap(),
                                     type_: param_type,
                                     borrow: ISLBorrowRule::IslKeep });
      }
      _ => bail!("Expect a func decl's inner to be a param."),
    }
  }
  println!(".");

  return Ok(ISLFunction { name: func_decl.name.clone(),
                          parameters: vec![],
                          ret_type: CType::I32 });
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
                                         .begin
                                         .spelling_loc
                                         .unwrap()
                                         .file
                                         .to_string();
        if func_decl.name.starts_with("isl_") {
          if spelling_filename.starts_with("isl/include/") {
            // println!("Processing func {} in file {}.",
            //          func_decl.name, spelling_filename);
            isl_functions.push(get_function_from_decl(&func_decl, &decl.inner, filename, state)?);
          }
        }
      }
      _ => {}
    }
  }
  return Ok(isl_functions);
}
