use anyhow::{bail, Result};
pub enum CType {
  Void,
  Int,
}

pub enum ISLBorrowRule {
  IslKeep,
  IslTake,
  PassByValue,
}

pub struct Parameter {
  pub name: String,
  pub type_: CType,
  pub borrow: ISLBorrowRule,
}

pub struct ISLFunction {
  pub name: String,
  pub parameters: Vec<Parameter>,
  pub ret_type: CType,
}

pub fn ctype_from_string(s: &String) -> Result<CType> {
  match s.as_str() {
    "void" => Ok(CType::Void),
    "int" => Ok(CType::Int),
    _ => bail!(format!("Unknown ctype {}", s)),
  }
}