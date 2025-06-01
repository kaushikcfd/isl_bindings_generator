use crate::types::ISLFunction;
use anyhow::Result;
use serde_derive::Deserialize;
use std::process::Command;

pub type Node = clang_ast::Node<Clang>;

#[derive(Deserialize, Debug)]
pub enum Clang {
  TranslationUnitDecl(TranslationUnitDecl),
  EnumDecl(EnumDecl),
  FunctionDecl(FunctionDecl),
  ParmVarDecl(ParmVarDecl),
  Unknown,
}

#[derive(Deserialize, Debug)]
pub struct TranslationUnitDecl {
  pub loc: Option<clang_ast::SourceLocation>,
  pub range: Option<clang_ast::SourceRange>,
}

#[derive(Deserialize, Debug)]
pub struct FunctionDecl {
  pub name: String,
  #[serde(rename = "mangledName", default)]
  pub mangled_name: String,
  pub loc: Option<clang_ast::SourceLocation>,
  pub range: Option<clang_ast::SourceRange>,
}

#[derive(Deserialize, Debug)]
pub struct ParmVarDecl {
  pub name: Option<String>,
  #[serde(rename = "type", default)]
  pub type_: Type,
  pub loc: Option<clang_ast::SourceLocation>,
  pub range: Option<clang_ast::SourceRange>,
}

#[derive(Deserialize, Debug)]
pub struct Type {
  #[serde(rename = "qualType", default)]
  pub qual_type: String,
}

impl Default for Type {
  fn default() -> Self {
    return Type { qual_type: String::from("") };
  }
}

#[derive(Deserialize, Debug)]
pub struct EnumDecl {
  pub name: Option<String>,
  pub loc: Option<clang_ast::SourceLocation>,
  pub range: Option<clang_ast::SourceRange>,
}

fn cfile_to_json(file: &str) -> Result<String> {
  // Run clang command
  let output = Command::new("clang").args(["-I",
                                           "isl/include/",
                                           "-I",
                                           "/usr/lib/llvm-19/lib/clang/19/include/",
                                           "-Xclang",
                                           "-ast-dump=json",
                                           "-fsyntax-only",
                                           file])
                                    .output()?;

  // Check if command was successful
  if !output.status.success() {
    anyhow::bail!("clang failed with status {}: {}",
                  output.status,
                  String::from_utf8_lossy(&output.stderr));
  }
  let json_content = String::from_utf8(output.stdout)?;
  return Ok(json_content);
}

pub fn extract_functions(filename: &str) -> Vec<ISLFunction> {
  let ast_json = cfile_to_json(filename).unwrap();
  let node: Node = serde_json::from_str(&ast_json.as_str()).unwrap();
  println!("{:#?}", node);
  return vec![];
}
