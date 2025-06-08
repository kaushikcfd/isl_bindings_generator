use anyhow::{bail, Result};
use std::fmt;

use crate::utils::guard_identifier;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CType {
  Void,
  ISLBool,
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
  ISLOptions,
  ISLDimType,
  ISLError,
  ISLFold,
  ISLStat,
  ISLAff,
  ISLAffList,
  ISLMultiAff,
  ISLLocalSpace,
  ISLSpace,
  ISLId,
  ISLIdList,
  ISLMultiId,
  ISLBasicSet,
  ISLBasicSetList,
  ISLBasicMap,
  ISLBasicMapList,
  ISLPrinter,
  ISLSet,
  ISLTerm,
  ISLUnionSet,
  ISLUnionSetList,
  ISLSetList,
  ISLMap,
  ISLUnionMap,
  ISLUnionMapList,
  ISLMapList,
  ISLMultiVal,
  ISLVal,
  ISLValList,
  ISLVec,
  ISLMat,
  ISLPoint,
  ISLConstraint,
  ISLConstraintList,
  ISLStrideInfo,
  ISLFixedBox,
  ISLUnionPwAff,
  ISLUnionPwAffList,
  ISLUnionPwMultiAffList,
  ISLPwAff,
  ISLPwAffList,
  ISLPwMultiAff,
  ISLPwMultiAffList,
  ISLUnionPwMultiAff,
  ISLMultiPwAff,
  ISLMultiUnionPwAff,
  ISLQPolynomial,
  ISLQPolynomialList,
  ISLQPolynomialFold,
  ISLPwQPolynomialFold,
  ISLUnionPwQPolynomialFold,
  ISLPwQPolynomialFoldList,
  ISLPwQPolynomial,
  ISLPwQPolynomialList,
  ISLUnionPwQPolynomial,
  ISLSchedule,
  ISLScheduleNode,
  ISLScheduleConstaints,
  Unsupported,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug, Copy)]
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

impl Parameter {
  pub fn make_identifier_rust_legal(&self) -> Self {
    Parameter { name: guard_identifier(&self.name),
                type_: self.type_,
                borrow: self.borrow.clone() }
  }
}

#[derive(Hash, PartialEq, Eq)]
pub struct ISLFunction {
  pub name: String,
  pub parameters: Vec<Parameter>,
  pub ret_type: CType,
}

impl ISLFunction {
  pub fn has_all_known_types(&self) -> bool {
    !self.parameters
         .iter()
         .any(|p| p.type_ == CType::Unsupported)
  }

  pub fn make_identifiers_rust_legal(self) -> Self {
    return ISLFunction { name: self.name,
                         parameters: self.parameters
                                         .iter()
                                         .map(|p| p.make_identifier_rust_legal())
                                         .collect(),
                         ret_type: self.ret_type };
  }
}

#[derive(Hash, PartialEq, Eq)]
pub struct ISLEnum {
  pub name: String,
  pub variants: Vec<String>,
  pub values: Vec<i32>,
}

impl ISLEnum {
  pub fn new<N, V, VI, I>(name: N, variants: V, values: I) -> Self
    where N: ToString,
          V: IntoIterator<Item = VI>,
          VI: ToString,
          I: IntoIterator<Item = i32>
  {
    let variants: Vec<String> = variants.into_iter().map(|v| v.to_string()).collect();
    let values: Vec<i32> = values.into_iter().collect();
    assert_eq!(variants.len(), values.len());
    ISLEnum { name: name.to_string(),
              variants: variants,
              values: values }
  }
}

impl fmt::Display for ISLEnum {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}({})", self.name, self.variants.join(", "))
  }
}

impl fmt::Display for ISLFunction {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let param_strs: Vec<String> =
      self.parameters
          .iter()
          .map(|p| format!("{} {:#?} {}", p.borrow, p.type_, p.name).to_string())
          .collect();
    write!(f,
           "{:#?} {}({})",
           self.ret_type,
           self.name,
           param_strs.join(", "))
  }
}

pub fn ctype_from_string<S>(s: &S) -> Result<CType>
  where S: ToString
{
  let s = s.to_string();
  match s.trim() {
    "int" => Ok(CType::I32),
    "unsigned int" => Ok(CType::U32),
    "long" => Ok(CType::I64),
    "unsigned long" => Ok(CType::U64),
    "float" => Ok(CType::F32),
    "double" => Ok(CType::F64),
    "isl_bool" => Ok(CType::ISLBool),
    "struct isl_args *" => Ok(CType::ISLArgs),
    "isl_ctx *" | "struct isl_ctx *" => Ok(CType::ISLCtx),
    "isl_id *" | "struct isl_id *" => Ok(CType::ISLId),
    "isl_stride_info *" => Ok(CType::ISLStrideInfo),
    "isl_fixed_box *" => Ok(CType::ISLFixedBox),
    "isl_vec *" => Ok(CType::ISLVec),
    "isl_mat *" => Ok(CType::ISLMat),
    "isl_constraint *" | "struct isl_constraint *" => Ok(CType::ISLConstraint),
    "isl_constraint_list *" | "struct isl_constraint_list *" => Ok(CType::ISLConstraintList),
    "isl_schedule *" | "struct isl_schedule *" => Ok(CType::ISLSchedule),
    "isl_schedule_node *" | "struct isl_schedule_node *" => Ok(CType::ISLScheduleNode),
    "isl_schedule_constraints *" | "struct isl_schedule_constraints *" => {
      Ok(CType::ISLScheduleConstaints)
    }
    "isl_point *" | "struct isl_point *" => Ok(CType::ISLPoint),
    "isl_id_list *" | "struct isl_id_list *" => Ok(CType::ISLIdList),
    "isl_multi_id *" | "struct isl_multi_id *" => Ok(CType::ISLMultiId),
    "isl_basic_set *" | "struct isl_basic_set *" | "const isl_basic_set *" => {
      Ok(CType::ISLBasicSet)
    }
    "isl_basic_set_list *" | "struct isl_basic_set_list *" => Ok(CType::ISLBasicSetList),
    "isl_set *" | "struct isl_set *" => Ok(CType::ISLSet),
    "isl_term *" | "struct isl_term *" => Ok(CType::ISLTerm),
    "isl_union_set *" | "struct isl_union_set *" => Ok(CType::ISLUnionSet),
    "isl_union_set_list *" | "struct isl_union_set_list *" => Ok(CType::ISLUnionSetList),
    "isl_set_list *" | "struct isl_set_list *" => Ok(CType::ISLSetList),
    "isl_basic_map *" | "struct isl_basic_map *" | "const isl_basic_map *" => {
      Ok(CType::ISLBasicMap)
    }
    "isl_basic_map_list *" | "struct isl_basic_map_list *" => Ok(CType::ISLBasicMapList),
    "isl_map *" | "struct isl_map *" => Ok(CType::ISLMap),
    "isl_union_map *" | "struct isl_union_map *" => Ok(CType::ISLUnionMap),
    "isl_union_map_list *" | "struct isl_union_map_list *" => Ok(CType::ISLUnionMapList),
    "isl_map_list *" | "struct isl_map_list *" => Ok(CType::ISLMapList),
    "isl_printer *" | "struct isl_printer *" => Ok(CType::ISLPrinter),
    "isl_val *" | "struct isl_val *" => Ok(CType::ISLVal),
    "isl_val_list *" | "struct isl_val_list *" => Ok(CType::ISLValList),
    "isl_multi_val *" | "struct isl_multi_val *" => Ok(CType::ISLMultiVal),
    "size_t" => Ok(CType::Sizet),
    "enum isl_fold" => Ok(CType::ISLFold),
    "enum isl_error" => Ok(CType::ISLError),
    "enum isl_dim_type" => Ok(CType::ISLDimType),
    "const char *" | "char *" => Ok(CType::CString),
    "uint32_t" => Ok(CType::U32),
    "void" => Ok(CType::Void),
    "isl_stat" => Ok(CType::ISLStat),
    "isl_space *" => Ok(CType::ISLSpace),
    "isl_local_space *" => Ok(CType::ISLLocalSpace),
    "isl_aff *" | "struct isl_aff *" => Ok(CType::ISLAff),
    "isl_qpolynomial *" | "struct isl_qpolynomial *" => Ok(CType::ISLQPolynomial),
    "isl_qpolynomial_list *" | "struct isl_qpolynomial_list *" => Ok(CType::ISLQPolynomialList),
    "isl_qpolynomial_fold *" => Ok(CType::ISLQPolynomialFold),
    "isl_pw_qpolynomial_fold *" | "struct isl_pw_qpolynomial_fold *" => {
      Ok(CType::ISLPwQPolynomialFold)
    }
    "isl_union_pw_qpolynomial_fold *" => Ok(CType::ISLUnionPwQPolynomialFold),
    "isl_pw_qpolynomial_fold_list *" | "struct isl_pw_qpolynomial_fold_list *" => {
      Ok(CType::ISLPwQPolynomialFoldList)
    }
    "isl_aff_list *" | "struct isl_aff_list *" => Ok(CType::ISLAffList),
    "isl_pw_aff *" | "struct isl_pw_aff *" => Ok(CType::ISLPwAff),
    "isl_pw_qpolynomial *" | "struct isl_pw_qpolynomial *" => Ok(CType::ISLPwQPolynomial),
    "isl_pw_qpolynomial_list *" | "struct isl_pw_qpolynomial_list *" => {
      Ok(CType::ISLPwQPolynomialList)
    }
    "isl_union_pw_qpolynomial *" | "struct isl_union_pw_qpolynomial *" => {
      Ok(CType::ISLUnionPwQPolynomial)
    }
    "isl_union_pw_aff *" | "struct isl_union_pw_aff *" => Ok(CType::ISLUnionPwAff),
    "isl_union_pw_aff_list *" | "struct isl_union_pw_aff_list *" => Ok(CType::ISLUnionPwAffList),
    "isl_union_pw_multi_aff_list *" | "struct isl_union_pw_multi_aff_list *" => {
      Ok(CType::ISLUnionPwMultiAffList)
    }
    "isl_pw_aff_list *" | "struct isl_pw_aff_list *" => Ok(CType::ISLPwAffList),
    "isl_multi_aff *" => Ok(CType::ISLMultiAff),
    "isl_pw_multi_aff *" | "struct isl_pw_multi_aff *" => Ok(CType::ISLPwMultiAff),
    "isl_pw_multi_aff_list *" | "struct isl_pw_multi_aff_list *" => Ok(CType::ISLPwMultiAffList),
    "isl_union_pw_multi_aff *" | "struct isl_union_pw_multi_aff *" => Ok(CType::ISLUnionPwMultiAff),
    "isl_multi_pw_aff *" => Ok(CType::ISLMultiPwAff),
    "isl_multi_union_pw_aff *" => Ok(CType::ISLMultiUnionPwAff),
    "struct isl_options *" => Ok(CType::ISLOptions),
    "isl_size" => Ok(CType::I32),
    "void *"
    | "const void *"
    | "char **"
    | "int *"
    | "isl_local_space **"
    | "isl_qpolynomial **"
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
    | "isl_set **"
    | "isl_basic_set *(*)(isl_basic_set *, void *)"
    | "isl_set *(*)(isl_set *, void *)"
    | "int (*)(struct isl_basic_set *, struct isl_basic_set *, void *)"
    | "int (*)(struct isl_set *, struct isl_set *, void *)"
    | "isl_stat (*)(isl_id *, void *)"
    | "isl_bool (*)(isl_id *, void *)"
    | "isl_id *(*)(isl_id *, void *)"
    | "int (*)(struct isl_id *, struct isl_id *, void *)"
    | "isl_bool (*)(isl_id *, isl_id *, void *)"
    | "isl_stat (*)(isl_id_list *, void *)"
    | "isl_val *(*)(isl_val *, void *)"
    | "int (*)(struct isl_val *, struct isl_val *, void *)"
    | "void (*)(void *)"
    | "isl_mat **"
    | "isl_constraint **"
    | "isl_stat (*)(isl_constraint *, void *)"
    | "isl_stat (*)(isl_constraint_list *, void *)"
    | "isl_bool (*)(isl_constraint *, void *)"
    | "isl_bool (*)(isl_constraint *, isl_constraint *, void *)"
    | "isl_constraint *(*)(isl_constraint *, void *)"
    | "int (*)(struct isl_constraint *, struct isl_constraint *, void *)"
    | "isl_stat (*)(isl_constraint *, isl_constraint *, isl_basic_set *, void *)"
    | "isl_stat (*)(isl_point *, void *)"
    | "isl_stat (*)(isl_basic_map *, void *)"
    | "isl_bool (*)(isl_basic_map *, void *)"
    | "isl_basic_map *(*)(isl_basic_map *, void *)"
    | "int (*)(struct isl_basic_map *, struct isl_basic_map *, void *)"
    | "isl_bool (*)(isl_basic_map *, isl_basic_map *, void *)"
    | "isl_stat (*)(isl_basic_map_list *, void *)"
    | "isl_stat (*)(isl_map *, void *)"
    | "isl_bool (*)(isl_map *, void *)"
    | "isl_map *(*)(isl_map *, void *)"
    | "int (*)(struct isl_map *, struct isl_map *, void *)"
    | "isl_bool (*)(isl_map *, isl_map *, void *)"
    | "isl_stat (*)(isl_map_list *, void *)"
    | "isl_stat (*)(isl_union_map *, void *)"
    | "isl_bool (*)(isl_union_map *, void *)"
    | "isl_union_map *(*)(isl_union_map *, void *)"
    | "int (*)(struct isl_union_map *, struct isl_union_map *, void *)"
    | "isl_bool (*)(isl_union_map *, isl_union_map *, void *)"
    | "isl_stat (*)(isl_union_map_list *, void *)"
    | "isl_stat (*)(isl_union_set *, void *)"
    | "isl_bool (*)(isl_union_set *, void *)"
    | "isl_union_set *(*)(isl_union_set *, void *)"
    | "int (*)(struct isl_union_set *, struct isl_union_set *, void *)"
    | "isl_bool (*)(isl_union_set *, isl_union_set *, void *)"
    | "isl_stat (*)(isl_union_set_list *, void *)"
    | "isl_stat (*)(isl_set *, isl_aff *, void *)"
    | "isl_bool (*)(isl_set *, isl_aff *, void *)"
    | "isl_bool *"
    | "isl_stat (*)(isl_set *, isl_multi_aff *, void *)"
    | "isl_bool (*)(isl_set *, isl_multi_aff *, void *)"
    | "isl_stat (*)(isl_pw_multi_aff *, void *)"
    | "isl_bool (*)(isl_pw_multi_aff *, void *)"
    | "isl_stat (*)(isl_pw_aff *, void *)"
    | "isl_bool (*)(isl_pw_aff *, void *)"
    | "isl_stat (*)(isl_aff *, void *)"
    | "isl_bool (*)(isl_aff *, void *)"
    | "isl_aff *(*)(isl_aff *, void *)"
    | "int (*)(struct isl_aff *, struct isl_aff *, void *)"
    | "isl_bool (*)(isl_aff *, isl_aff *, void *)"
    | "isl_stat (*)(isl_aff_list *, void *)"
    | "isl_pw_aff *(*)(isl_pw_aff *, void *)"
    | "int (*)(struct isl_pw_aff *, struct isl_pw_aff *, void *)"
    | "isl_bool (*)(isl_pw_aff *, isl_pw_aff *, void *)"
    | "isl_stat (*)(isl_pw_aff_list *, void *)"
    | "isl_pw_multi_aff *(*)(isl_pw_multi_aff *, void *)"
    | "int (*)(struct isl_pw_multi_aff *, struct isl_pw_multi_aff *, void *)"
    | "isl_bool (*)(isl_pw_multi_aff *, isl_pw_multi_aff *, void *)"
    | "isl_stat (*)(isl_pw_multi_aff_list *, void *)"
    | "isl_stat (*)(isl_union_pw_aff *, void *)"
    | "isl_bool (*)(isl_union_pw_aff *, void *)"
    | "isl_union_pw_aff *(*)(isl_union_pw_aff *, void *)"
    | "int (*)(struct isl_union_pw_aff *, struct isl_union_pw_aff *, void *)"
    | "isl_bool (*)(isl_union_pw_aff *, isl_union_pw_aff *, void *)"
    | "isl_stat (*)(isl_union_pw_aff_list *, void *)"
    | "isl_stat (*)(isl_union_pw_multi_aff *, void *)"
    | "isl_bool (*)(isl_union_pw_multi_aff *, void *)"
    | "isl_union_pw_multi_aff *(*)(isl_union_pw_multi_aff *, void *)"
    | "int (*)(struct isl_union_pw_multi_aff *, struct isl_union_pw_multi_aff *, void *)"
    | "isl_bool (*)(isl_union_pw_multi_aff *, isl_union_pw_multi_aff *, void *)"
    | "isl_stat (*)(isl_union_pw_multi_aff_list *, void *)"
    | "isl_stat (*)(isl_basic_set *, isl_qpolynomial *, void *)"
    | "isl_stat (*)(isl_term *, void *)"
    | "isl_bool (*)(isl_term *, void *)"
    | "isl_term *(*)(isl_term *, void *)"
    | "int (*)(struct isl_term *, struct isl_term *, void *)"
    | "isl_bool (*)(isl_term *, isl_term *, void *)"
    | "isl_stat (*)(isl_set *, isl_qpolynomial *, void *)"
    | "isl_bool (*)(isl_set *, isl_qpolynomial *, void *)"
    | "isl_stat (*)(isl_qpolynomial *, void *)"
    | "isl_pw_qpolynomial *(*)(isl_basic_set *)"
    | "isl_stat (*)(isl_set *, isl_qpolynomial_fold *, void *)"
    | "isl_bool (*)(isl_set *, isl_qpolynomial_fold *, void *)"
    | "isl_stat (*)(isl_pw_qpolynomial *, void *)"
    | "isl_bool (*)(isl_pw_qpolynomial *, void *)"
    | "isl_stat (*)(isl_pw_qpolynomial_fold *, void *)"
    | "isl_bool (*)(isl_pw_qpolynomial_fold *, void *)"
    | "isl_bool (*)(isl_qpolynomial *, void *)"
    | "isl_qpolynomial *(*)(isl_qpolynomial *, void *)"
    | "int (*)(struct isl_qpolynomial *, struct isl_qpolynomial *, void *)"
    | "isl_bool (*)(isl_qpolynomial *, isl_qpolynomial *, void *)"
    | "isl_stat (*)(isl_qpolynomial_list *, void *)"
    | "isl_pw_qpolynomial *(*)(isl_pw_qpolynomial *, void *)"
    | "int (*)(struct isl_pw_qpolynomial *, struct isl_pw_qpolynomial *, void *)"
    | "isl_bool (*)(isl_pw_qpolynomial *, isl_pw_qpolynomial *, void *)"
    | "isl_stat (*)(isl_pw_qpolynomial_list *, void *)"
    | "isl_pw_qpolynomial_fold *(*)(isl_pw_qpolynomial_fold *, void *)"
    | "int (*)(struct isl_pw_qpolynomial_fold *, struct isl_pw_qpolynomial_fold *, void *)"
    | "isl_bool (*)(isl_pw_qpolynomial_fold *, isl_pw_qpolynomial_fold *, void *)"
    | "isl_stat (*)(isl_pw_qpolynomial_fold_list *, void *)"
    | "isl_bool (*)(isl_schedule_node *, void *)"
    | "isl_schedule_node *(*)(isl_schedule_node *, void *)"
    | "FILE *" => Ok(CType::Unsupported),
    _ => bail!(format!("Unknown ctype '{}'.", s)),
  }
}

pub fn is_primitive_ctype(type_: CType) -> bool {
  match type_ {
    CType::ISLBool
    | CType::I32
    | CType::U32
    | CType::I64
    | CType::U64
    | CType::F32
    | CType::F64
    | CType::Sizet
    | CType::ISLDimType
    | CType::ISLFold
    | CType::ISLError => true,
    _ => false,
  }
}

pub fn get_rust_typename(type_: CType) -> Result<&'static str> {
  match type_ {
    CType::Void => Ok("()"),
    CType::ISLBool => Ok("bool"),
    CType::I32 => Ok("i32"),
    CType::U32 => Ok("u32"),
    CType::I64 => Ok("i64"),
    CType::U64 => Ok("u64"),
    CType::F32 => Ok("f32"),
    CType::F64 => Ok("f64"),
    CType::Sizet => Ok("usize"),
    CType::CString => Ok("&str"),
    CType::ISLArgs => Ok("Args"),
    CType::ISLCtx => Ok("Context"),
    CType::ISLOptions => Ok("Options"),
    CType::ISLDimType => Ok("DimType"),
    CType::ISLError => Ok("Error"),
    CType::ISLFold => Ok("Fold"),
    CType::ISLStat => Ok("Stat"),
    CType::ISLAff => Ok("Aff"),
    CType::ISLAffList => Ok("AffList"),
    CType::ISLMultiAff => Ok("MultiAff"),
    CType::ISLLocalSpace => Ok("LocalSpace"),
    CType::ISLSpace => Ok("Space"),
    CType::ISLId => Ok("Id"),
    CType::ISLIdList => Ok("IdList"),
    CType::ISLMultiId => Ok("MultiId"),
    CType::ISLBasicSet => Ok("BasicSet"),
    CType::ISLBasicSetList => Ok("BasicSetList"),
    CType::ISLBasicMap => Ok("BasicMap"),
    CType::ISLBasicMapList => Ok("BasicMapList"),
    CType::ISLPrinter => Ok("Printer"),
    CType::ISLSet => Ok("Set"),
    CType::ISLTerm => Ok("Term"),
    CType::ISLUnionSet => Ok("UnionSet"),
    CType::ISLUnionSetList => Ok("UnionSetList"),
    CType::ISLSetList => Ok("SetList"),
    CType::ISLMap => Ok("Map"),
    CType::ISLUnionMap => Ok("UnionMap"),
    CType::ISLUnionMapList => Ok("UnionMapList"),
    CType::ISLMapList => Ok("MapList"),
    CType::ISLMultiVal => Ok("MultiVal"),
    CType::ISLVal => Ok("Val"),
    CType::ISLValList => Ok("ValList"),
    CType::ISLVec => Ok("Vec"),
    CType::ISLMat => Ok("Mat"),
    CType::ISLPoint => Ok("Point"),
    CType::ISLConstraint => Ok("Constraint"),
    CType::ISLConstraintList => Ok("ConstraintList"),
    CType::ISLStrideInfo => Ok("StrideInfo"),
    CType::ISLFixedBox => Ok("FixedBox"),
    CType::ISLUnionPwAff => Ok("UnionPwAff"),
    CType::ISLUnionPwAffList => Ok("UnionPwAffList"),
    CType::ISLUnionPwMultiAffList => Ok("UnionPwMultiAffList"),
    CType::ISLPwAff => Ok("PwAff"),
    CType::ISLPwAffList => Ok("PwAffList"),
    CType::ISLPwMultiAff => Ok("PwMultiAff"),
    CType::ISLPwMultiAffList => Ok("PwMultiAffList"),
    CType::ISLUnionPwMultiAff => Ok("UnionPwMultiAff"),
    CType::ISLMultiPwAff => Ok("MultiPwAff"),
    CType::ISLMultiUnionPwAff => Ok("MultiUnionPwAff"),
    CType::ISLQPolynomial => Ok("QPolynomial"),
    CType::ISLQPolynomialList => Ok("QPolynomialList"),
    CType::ISLQPolynomialFold => Ok("QPolynomialFold"),
    CType::ISLPwQPolynomialFold => Ok("PwQPolynomialFold"),
    CType::ISLUnionPwQPolynomialFold => Ok("UnionPwQPolynomialFold"),
    CType::ISLPwQPolynomialFoldList => Ok("PwQPolynomialFoldList"),
    CType::ISLPwQPolynomial => Ok("PwQPolynomial"),
    CType::ISLPwQPolynomialList => Ok("PwQPolynomialList"),
    CType::ISLUnionPwQPolynomial => Ok("UnionPwQPolynomial"),
    CType::ISLSchedule => Ok("Schedule"),
    CType::ISLScheduleNode => Ok("ScheduleNode"),
    CType::ISLScheduleConstaints => Ok("ScheduleConstaints"),
    CType::Unsupported => bail!("Cannot convert this type to rust!"),
  }
}

/// Returns the name for `c_arg_t` to use in `extern "C"` block function
/// declarations.
pub fn get_typename_in_extern_block(type_: CType) -> Result<&'static str> {
  match type_ {
    CType::Void
    | CType::I32
    | CType::U32
    | CType::I64
    | CType::U64
    | CType::F32
    | CType::F64
    | CType::Sizet
    | CType::ISLDimType
    | CType::ISLError
    | CType::ISLFold
    | CType::ISLStat => get_rust_typename(type_),
    CType::ISLBool => Ok("i32"),
    CType::CString => Ok("*const c_char"),
    CType::ISLArgs
    | CType::ISLCtx
    | CType::ISLOptions
    | CType::ISLAff
    | CType::ISLAffList
    | CType::ISLMultiAff
    | CType::ISLLocalSpace
    | CType::ISLSpace
    | CType::ISLId
    | CType::ISLIdList
    | CType::ISLMultiId
    | CType::ISLBasicSet
    | CType::ISLBasicSetList
    | CType::ISLBasicMap
    | CType::ISLBasicMapList
    | CType::ISLPrinter
    | CType::ISLSet
    | CType::ISLTerm
    | CType::ISLUnionSet
    | CType::ISLUnionSetList
    | CType::ISLSetList
    | CType::ISLMap
    | CType::ISLUnionMap
    | CType::ISLUnionMapList
    | CType::ISLMapList
    | CType::ISLMultiVal
    | CType::ISLVal
    | CType::ISLValList
    | CType::ISLVec
    | CType::ISLMat
    | CType::ISLPoint
    | CType::ISLConstraint
    | CType::ISLConstraintList
    | CType::ISLStrideInfo
    | CType::ISLFixedBox
    | CType::ISLUnionPwAff
    | CType::ISLUnionPwAffList
    | CType::ISLUnionPwMultiAffList
    | CType::ISLPwAff
    | CType::ISLPwAffList
    | CType::ISLPwMultiAff
    | CType::ISLPwMultiAffList
    | CType::ISLUnionPwMultiAff
    | CType::ISLMultiPwAff
    | CType::ISLMultiUnionPwAff
    | CType::ISLQPolynomial
    | CType::ISLQPolynomialList
    | CType::ISLQPolynomialFold
    | CType::ISLPwQPolynomialFold
    | CType::ISLUnionPwQPolynomialFold
    | CType::ISLPwQPolynomialFoldList
    | CType::ISLPwQPolynomial
    | CType::ISLPwQPolynomialList
    | CType::ISLUnionPwQPolynomial
    | CType::ISLSchedule
    | CType::ISLScheduleNode
    | CType::ISLScheduleConstaints => Ok("uintptr_t"),
    CType::Unsupported => bail!("Cannot convert this type to rust!"),
  }
}

pub fn get_isl_struct_name(type_: CType) -> Result<&'static str> {
  match type_ {
    CType::ISLArgs => Ok("isl_args"),
    CType::ISLCtx => Ok("isl_ctx"),
    CType::ISLOptions => Ok("isl_options"),
    CType::ISLAff => Ok("isl_aff"),
    CType::ISLAffList => Ok("isl_aff_list"),
    CType::ISLMultiAff => Ok("isl_multi_aff"),
    CType::ISLLocalSpace => Ok("isl_local_space"),
    CType::ISLSpace => Ok("isl_space"),
    CType::ISLId => Ok("isl_id"),
    CType::ISLIdList => Ok("isl_id_list"),
    CType::ISLMultiId => Ok("isl_multi_id"),
    CType::ISLBasicSet => Ok("isl_basic_set"),
    CType::ISLBasicSetList => Ok("isl_basic_set_list"),
    CType::ISLBasicMap => Ok("isl_basic_map"),
    CType::ISLBasicMapList => Ok("isl_basic_map_list"),
    CType::ISLPrinter => Ok("isl_printer"),
    CType::ISLSet => Ok("isl_set"),
    CType::ISLTerm => Ok("isl_term"),
    CType::ISLUnionSet => Ok("isl_union_set"),
    CType::ISLUnionSetList => Ok("isl_union_set_list"),
    CType::ISLSetList => Ok("isl_set_list"),
    CType::ISLMap => Ok("isl_map"),
    CType::ISLUnionMap => Ok("isl_union_map"),
    CType::ISLUnionMapList => Ok("isl_union_map_list"),
    CType::ISLMapList => Ok("isl_map_list"),
    CType::ISLMultiVal => Ok("isl_multi_val"),
    CType::ISLVal => Ok("isl_val"),
    CType::ISLValList => Ok("isl_val_list"),
    CType::ISLVec => Ok("isl_vec"),
    CType::ISLMat => Ok("isl_mat"),
    CType::ISLPoint => Ok("isl_point"),
    CType::ISLConstraint => Ok("isl_constraint"),
    CType::ISLConstraintList => Ok("isl_constraint_list"),
    CType::ISLStrideInfo => Ok("isl_stride_info"),
    CType::ISLFixedBox => Ok("isl_fixed_box"),
    CType::ISLUnionPwAff => Ok("isl_union_pw_aff"),
    CType::ISLUnionPwAffList => Ok("isl_union_pw_aff_list"),
    CType::ISLUnionPwMultiAffList => Ok("isl_union_pw_multi_aff_list"),
    CType::ISLPwAff => Ok("isl_pw_aff"),
    CType::ISLPwAffList => Ok("isl_pw_aff_list"),
    CType::ISLPwMultiAff => Ok("isl_pw_multi_aff"),
    CType::ISLPwMultiAffList => Ok("isl_pw_multi_aff_list"),
    CType::ISLUnionPwMultiAff => Ok("isl_union_pw_multi_aff"),
    CType::ISLMultiPwAff => Ok("isl_multi_pw_aff"),
    CType::ISLMultiUnionPwAff => Ok("isl_multi_union_pw_aff"),
    CType::ISLQPolynomial => Ok("isl_qpolynomial"),
    CType::ISLQPolynomialList => Ok("isl_qpolynomial_list"),
    CType::ISLQPolynomialFold => Ok("isl_qpolynomial_fold"),
    CType::ISLPwQPolynomialFold => Ok("isl_pw_qpolynomial_fold"),
    CType::ISLUnionPwQPolynomialFold => Ok("isl_union_pw_qpolynomial_fold"),
    CType::ISLPwQPolynomialFoldList => Ok("isl_pw_qpolynomial_fold_list"),
    CType::ISLPwQPolynomial => Ok("isl_pw_qpolynomial"),
    CType::ISLPwQPolynomialList => Ok("isl_pw_qpolynomial_list"),
    CType::ISLUnionPwQPolynomial => Ok("isl_union_pw_qpolynomial"),
    CType::ISLSchedule => Ok("isl_schedule"),
    CType::ISLScheduleNode => Ok("isl_schedule_node"),
    _ => bail!("not a core ISL type."),
  }
}
