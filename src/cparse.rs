use crate::types::{ctype_from_string, CType, ISLBorrowRule, ISLFunction, Parameter};
use anyhow::{bail, Result};
use serde::Serialize;
use serde_derive::Deserialize;
use std::collections::HashMap;
use std::process::Command;

#[derive(Deserialize, Debug, Clone)]
struct ClangNode {
  pub kind: String,
  pub name: Option<String>,
  #[serde(rename = "type")]
  pub type_: Option<Type>,
  #[serde(default = "Vec::new")]
  pub inner: Vec<ClangNode>,
  pub loc: Option<SourceLocation>,
  pub range: Option<SourceRange>,
}

#[derive(Deserialize, Debug, Clone)]
struct Type {
  #[serde(rename = "qualType")]
  pub qual_type: String,
}

#[derive(Serialize, Deserialize, Default, Clone, Eq, PartialEq, Hash, Debug)]
struct SourceRange {
  pub begin: SourceLocation,
  pub end: SourceLocation,
}

#[derive(Serialize, Deserialize, Default, Clone, Eq, PartialEq, Hash, Debug)]
struct SourceLocation {
  #[serde(rename = "spellingLoc")]
  pub spelling_loc: Option<BareSourceLocation>,
  #[serde(rename = "expansionLoc")]
  pub expansion_loc: Option<BareSourceLocation>,
  pub offset: Option<usize>,
  pub file: Option<String>,
  pub line: Option<usize>,
  pub col: Option<usize>,
  #[serde(rename = "tokLen")]
  pub tok_len: Option<usize>,
}

#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Hash, Debug)]
struct BareSourceLocation {
  pub offset: usize,
  pub file: Option<String>,
  pub line: Option<usize>,
  pub col: usize,
  #[serde(rename = "tokLen")]
  pub tok_len: usize,
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

  println!("JSON={}", json_content);

  return Ok(json_content);
}

fn get_begin_range_spelling_loc_filename(node: &ClangNode, input_file: &String) -> String {
  let input_file = input_file.clone();
  match &node.range {
    Some(src_range) => match &src_range.begin.spelling_loc {
      Some(src_loc) => src_loc.file.clone().unwrap_or(input_file),
      None => src_range.begin.file.clone().unwrap_or(input_file),
    },
    None => input_file,
  }
}

fn get_begin_range_spelling_loc_for_func_decl(node: &ClangNode, parent_file: &String)
                                              -> Result<(String, usize, usize)> {
  let parent_file = parent_file.clone();
  println!("{:#?}", node.loc);
  match &node.loc {
    Some(src_loc) => match &src_loc.spelling_loc {
      Some(bare_src_loc) => {
        Ok((bare_src_loc.file.clone().unwrap_or(parent_file), bare_src_loc.line.unwrap(), bare_src_loc.col))
      }
      None => {
        match src_loc.line {
          Some(src_loc_line) =>  Ok((src_loc.file.clone().unwrap_or(parent_file), src_loc_line, src_loc.col.unwrap())),
          None => bail!(format!("Cannot get line number of function {:#?}.", node.name))
        }
      },
    },
    None => bail!("Cannot get location of function decl."),
  }
}

fn get_begin_range_spelling_loc_for_param_decl(node: &ClangNode, parent_file: &String,
                                               parent_line: usize)
                                               -> Result<(String, usize, usize)> {
  let parent_file = parent_file.clone();
  match &node.range {
    Some(src_range) => match &src_range.begin.spelling_loc {
      Some(src_loc) => Ok((src_loc.file.clone().unwrap_or(parent_file),
                           src_loc.line.unwrap_or(parent_line),
                           src_loc.col)),
      None => bail!("Canot get source range for param var decl."),
    },
    None => bail!("Canot get source range for param var decl."),
  }
}

fn get_function_from_decl(func_decl: &ClangNode, input_file: &String, state: &mut ParseState)
                          -> Result<ISLFunction> {
  assert!(func_decl.kind.as_str() == "FunctionDecl");
  let func_name = func_decl.name.clone().unwrap();
  let mut func_params: Vec<Parameter> = vec![];
  let (func_file, func_line, func_col) =
    get_begin_range_spelling_loc_for_func_decl(func_decl, input_file)?;
  print!("Function {} at ({}, {}, {}). Takes: ", func_name, func_file, func_line, func_col);

  for (iparam, param_decl) in func_decl.inner.iter().enumerate() {
    if param_decl.kind != "ParamDecl" {
      bail!("Expect a func decl's inner to be a param.");
    }
    let param_name = param_decl.name.clone().unwrap_or(format!("arg{}", iparam));
    let param_type = ctype_from_string(&param_decl.type_.clone().unwrap().qual_type)?;
    let (param_file, param_line, param_col) =
      get_begin_range_spelling_loc_for_param_decl(param_decl, &func_file, func_line)?;
    print!("{}[{}, {}, {}]",
           param_name, param_file, param_line, param_col);

    // FIXME: Care about borrowship rules.
    func_params.push(Parameter { name: param_name,
                                 type_: param_type,
                                 borrow: ISLBorrowRule::IslKeep });
  }
  println!(".");

  return Ok(ISLFunction { name: func_name,
                          parameters: vec![],
                          ret_type: CType::Void });
}

pub fn extract_functions(filename: &String, state: &mut ParseState) -> Result<Vec<ISLFunction>> {
  let ast_json = cfile_to_json(filename)?;
  let node: ClangNode = serde_json::from_str(&ast_json.as_str())?;
  // println!("node={:#?}", node);

  let t_unit_body: Result<Vec<ClangNode>> = match node.kind.as_str() {
    "TranslationUnitDecl" => Ok(node.inner),
    _ => bail!("Parsed file not a translation unit?"),
  };

  let mut isl_functions: Vec<ISLFunction> = vec![];

  for decl in t_unit_body? {
    match decl.kind.as_str() {
      "FunctionDecl" => {
        let func_name = decl.name.clone().unwrap();
        if func_name.starts_with("isl_") {
          let spelling_filename = get_begin_range_spelling_loc_filename(&decl, filename);
          if spelling_filename.starts_with("isl/include/") {
            isl_functions.push(get_function_from_decl(&decl, filename, state)?);
          }
        }
      }
      _ => {}
    }
  }
  return Ok(isl_functions);
}
