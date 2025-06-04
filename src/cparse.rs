use crate::types::ISLFunction;
use anyhow::{bail, Result};
use serde::Serialize;
use serde_derive::Deserialize;
use std::process::Command;

#[derive(Deserialize, Debug, Clone)]
pub struct ClangNode {
  pub kind: String,
  #[serde(default = "String::new")]
  pub name: String,
  #[serde(default = "Vec::new")]
  pub inner: Vec<ClangNode>,
  pub loc: Option<SourceLocation>,
  pub range: Option<SourceRange>,
}

#[derive(Serialize, Deserialize, Default, Clone, Eq, PartialEq, Hash, Debug)]
pub struct SourceRange {
  pub begin: SourceLocation,
  pub end: SourceLocation,
}

#[derive(Serialize, Deserialize, Default, Clone, Eq, PartialEq, Hash, Debug)]
pub struct SourceLocation {
  #[serde(rename = "spellingLoc")]
  pub spelling_loc: Option<BareSourceLocation>,
  #[serde(rename = "expansionLoc")]
  pub expansion_loc: Option<BareSourceLocation>,
}

#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Hash, Debug)]
pub struct BareSourceLocation {
  pub offset: usize,
  pub file: Option<String>,
  pub line: Option<usize>,
  pub col: usize,
  #[serde(rename = "tokLen")]
  pub tok_len: usize,
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
      Some(src_loc) => src_loc.file.clone().map_or(input_file, |f| f),
      None => input_file,
    },
    None => input_file,
  }
}

pub fn extract_functions(filename: &String) -> Result<Vec<ISLFunction>> {
  let ast_json = cfile_to_json(filename)?;
  let node: ClangNode = serde_json::from_str(&ast_json.as_str())?;
  // println!("node={:#?}", node);

  let t_unit_body: Result<Vec<ClangNode>> = match node.kind.as_str() {
    "TranslationUnitDecl" => Ok(node.inner),
    _ => bail!("Parsed file not a translation unit?"),
  };

  let isl_functions: Vec<ISLFunction> = vec![];

  for decl in t_unit_body? {
    match decl.kind.as_str() {
      "FunctionDecl" => {
        if decl.name.starts_with("isl_") {
          let spelling_filename = get_begin_range_spelling_loc_filename(&decl, filename);
          if spelling_filename.starts_with("isl/include/") {
            println!("Got a function: {} in file {}.",
                     decl.name, spelling_filename);
          }
        }
      }
      _ => {}
    }
  }
  return Ok(isl_functions);
}
