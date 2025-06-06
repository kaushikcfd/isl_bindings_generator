use anyhow::{bail, Result};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CType {
  Bool,
  I32,
  U32,
  I64,
  U64,
  F32,
  F64,
  Sizet,
  CString,
  ISLArgs,
  ISLCtx,
  ISLDimType,
  ISLError,
  ISLId,
  ISLBasicSet,
  ISLBasicSetList,
  ISLPrinter,
  ISLSet,
  ISLSetList,
  ISLMultiVal,
  ISLVal,
  ISLValList,
  Unsupported,
}

#[derive(Hash, PartialEq, Eq)]
pub enum ISLBorrowRule {
  IslKeep,
  IslTake,
  PassByValue,
  Unsupported,
}

impl fmt::Display for ISLBorrowRule {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      ISLBorrowRule::IslKeep => write!(f, "__isl_keep"),
      ISLBorrowRule::IslTake => write!(f, "__isl_take"),
      ISLBorrowRule::PassByValue => write!(f, "__isl_none"),
      ISLBorrowRule::Unsupported => write!(f, "unsupported"),
    }
  }
}

#[derive(Hash, PartialEq, Eq)]
pub struct Parameter {
  pub name: String,
  pub type_: CType,
  pub borrow: ISLBorrowRule,
}

#[derive(Hash, PartialEq, Eq)]
pub struct ISLFunction {
  pub name: String,
  pub parameters: Vec<Parameter>,
  pub ret_type: CType,
}

#[derive(Hash, PartialEq, Eq)]
pub struct ISLEnum {
  pub name: String,
  pub variants: Vec<String>,
}

impl fmt::Display for ISLEnum {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}({})", self.name, self.variants.join(", "))
  }
}

pub fn ctype_from_string(s: &String) -> Result<CType> {
  match s.as_str() {
    "int" => Ok(CType::I32),
    "unsigned int" => Ok(CType::U32),
    "long" => Ok(CType::I64),
    "unsigned long" => Ok(CType::U64),
    "float" => Ok(CType::F32),
    "double" => Ok(CType::F64),
    "isl_bool" => Ok(CType::Bool),
    "struct isl_args *" => Ok(CType::ISLArgs),
    "isl_ctx *" | "struct isl_ctx *" => Ok(CType::ISLCtx),
    "isl_id *" | "struct isl_id *" => Ok(CType::ISLId),
    "isl_basic_set *" | "struct isl_basic_set *" => Ok(CType::ISLBasicSet),
    "isl_basic_set_list *" | "struct isl_basic_set_list *" => Ok(CType::ISLBasicSetList),
    "isl_set *" | "struct isl_set *" => Ok(CType::ISLSet),
    "isl_set_list *" | "struct isl_set_list *" => Ok(CType::ISLSetList),
    "isl_printer *" | "struct isl_printer *" => Ok(CType::ISLPrinter),
    "isl_val *" | "struct isl_val *" => Ok(CType::ISLVal),
    "isl_val_list *" | "struct isl_val_list *" => Ok(CType::ISLValList),
    "isl_multi_val *" | "struct isl_multi_val *" => Ok(CType::ISLMultiVal),
    "size_t" => Ok(CType::Sizet),
    "enum isl_error" => Ok(CType::ISLError),
    "enum isl_dim_type" => Ok(CType::ISLDimType),
    "const char *" => Ok(CType::CString),
    "void *"
    | "const void *"
    | "char **"
    | "isl_stat (*)(isl_basic_set *, void *)"
    | "isl_stat (*)(isl_basic_set_list *, void *)"
    | "isl_bool (*)(isl_basic_set *, void *)"
    | "isl_bool (*)(isl_basic_set *, isl_basic_set *, void *)"
    | "isl_stat (*)(isl_set *, void *)"
    | "isl_stat (*)(isl_set_list *, void *)"
    | "isl_bool (*)(isl_set *, void *)"
    | "isl_bool (*)(isl_set *, isl_set *, void *)"
    | "isl_stat (*)(isl_val *, void *)"
    | "isl_stat (*)(isl_val_list *, void *)"
    | "isl_bool (*)(isl_val *, void *)"
    | "isl_bool (*)(isl_val *, isl_val *, void *)"
    | "isl_val **"
    | "FILE *" => Ok(CType::Unsupported),
    _ => bail!(format!("Unknown ctype '{}'.", s)),
  }
}

pub fn is_primitive_ctype(type_: CType) -> bool {
  match type_ {
    CType::Bool
    | CType::I32
    | CType::U32
    | CType::I64
    | CType::U64
    | CType::F32
    | CType::F64
    | CType::Sizet
    | CType::ISLDimType
    | CType::ISLError => true,
    _ => false,
  }
}
