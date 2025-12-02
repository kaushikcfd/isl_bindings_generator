// Copyright (c) 2025 Kaushik Kulkarni
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

use convert_case::{Case, Casing};
use std::collections::HashSet;

use anyhow::{bail, Result};
use codegen::{Function, Scope};

use crate::{
  types::{
    ctype_from_string, get_isl_struct_name, get_rust_typename, get_typename_in_extern_block,
    is_isl_struct_type, CType, ISLBorrowRule, ISLEnum, ISLFunction,
  },
  utils::guard_identifier,
};

fn imports_for_type(type_: CType, scope: &mut Scope) -> Result<()> {
  match type_ {
    CType::Void
    | CType::I32
    | CType::U32
    | CType::I64
    | CType::U64
    | CType::F32
    | CType::F64
    | CType::Sizet
    | CType::ISLBool => Ok(()),
    CType::CString => {
      scope.import("std::ffi", "CString");
      scope.import("std::ffi", "CStr");
      scope.import("std::os::raw", "c_char");
      Ok(())
    }
    CType::ISLCtx
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
    | CType::ISLRestriction
    | CType::ISLVec
    | CType::ISLMat
    | CType::ISLPoint
    | CType::ISLVertex
    | CType::ISLCell
    | CType::ISLVertices
    | CType::ISLConstraint
    | CType::ISLConstraintList
    | CType::ISLStrideInfo
    | CType::ISLASTExpr
    | CType::ISLASTNode
    | CType::ISLASTNodeList
    | CType::ISLASTExprList
    | CType::ISLIdToASTExpr
    | CType::ISLFlow
    | CType::ISLUnionFlow
    | CType::ISLAccessInfo
    | CType::ISLUnionAccessInfo
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
    | CType::ISLDimType
    | CType::ISLASTExprType
    | CType::ISLASTNodeType
    | CType::ISLASTExprOpType
    | CType::ISLArgType
    | CType::ISLScheduleNodeType
    | CType::ISLASTLoopType
    | CType::ISLError
    | CType::ISLFold
    | CType::ISLStat
    | CType::ISLScheduleConstraints => {
      scope.import("libc", "uintptr_t");
      scope.import("super", get_rust_typename(type_)?);
      Ok(())
    }
    CType::Unsupported => bail!("Cannot convert this type to rust!"),
  }
}

fn shadow_var_before_passing_to_isl_c(method: &mut Function, arg_t: CType, arg_name: &String,
                                      borrow: ISLBorrowRule)
                                      -> Result<()> {
  match arg_t {
    CType::Void
    | CType::I32
    | CType::U32
    | CType::I64
    | CType::U64
    | CType::F32
    | CType::F64
    | CType::Sizet
    | CType::ISLBool => {
      assert_eq!(borrow, ISLBorrowRule::PassByValue);
      Ok(())
    }
    CType::ISLDimType
    | CType::ISLASTExprType
    | CType::ISLASTNodeType
    | CType::ISLASTExprOpType
    | CType::ISLArgType
    | CType::ISLError
    | CType::ISLFold
    | CType::ISLStat
    | CType::ISLASTLoopType
    | CType::ISLScheduleNodeType => {
      assert_eq!(borrow, ISLBorrowRule::PassByValue);
      method.line(format!("let {} = {}.to_i32();", arg_name, arg_name));
      Ok(())
    }
    CType::CString => {
      assert_eq!(borrow, ISLBorrowRule::IslTake);
      method.line(format!("let {} = CString::new({}).unwrap();", arg_name, arg_name));
      method.line(format!("let {} = {}.as_ptr();", arg_name, arg_name));
      Ok(())
    }
    CType::ISLCtx
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
    | CType::ISLRestriction
    | CType::ISLVec
    | CType::ISLMat
    | CType::ISLPoint
    | CType::ISLVertex
    | CType::ISLCell
    | CType::ISLVertices
    | CType::ISLConstraint
    | CType::ISLConstraintList
    | CType::ISLStrideInfo
    | CType::ISLASTExpr
    | CType::ISLASTNode
    | CType::ISLASTNodeList
    | CType::ISLASTExprList
    | CType::ISLIdToASTExpr
    | CType::ISLFlow
    | CType::ISLUnionFlow
    | CType::ISLAccessInfo
    | CType::ISLUnionAccessInfo
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
    | CType::ISLScheduleConstraints => {
      match borrow {
        ISLBorrowRule::IslKeep => {
          method.line(format!("let {} = {}.ptr;", arg_name, arg_name));
        }
        ISLBorrowRule::IslTake => {
          method.line(format!("let mut {} = {};", arg_name, arg_name));
          method.line(format!("{}.do_not_free_on_drop();", arg_name));
          method.line(format!("let {} = {}.ptr;", arg_name, arg_name));
        }
        _ => bail!("Unexpected borrow"),
      }
      Ok(())
    }
    CType::Unsupported => bail!("Cannot convert this type to rust!"),
  }
}

fn shadow_return_from_isl_c(method: &mut Function, isl_func: &ISLFunction, return_var: &str)
                            -> Result<()> {
  match isl_func.ret_type {
    CType::Void
    | CType::I32
    | CType::U32
    | CType::I64
    | CType::U64
    | CType::F32
    | CType::F64
    | CType::Sizet => Ok(()),
    CType::ISLDimType
    | CType::ISLASTExprType
    | CType::ISLASTNodeType
    | CType::ISLASTExprOpType
    | CType::ISLArgType
    | CType::ISLError
    | CType::ISLFold
    | CType::ISLStat
    | CType::ISLASTLoopType
    | CType::ISLScheduleNodeType => {
      method.line(format!("let {} = {}::from_i32({});",
                          return_var,
                          get_rust_typename(isl_func.ret_type)?,
                          return_var));
      Ok(())
    }
    CType::ISLBool => {
      method.line(format!("let {} = match {} {{", return_var, return_var));
      method.line("    0 => false,");
      method.line("    1 => true,");
      method.line("    _ => { return Err(LibISLError::new(Error::Unknown, \"Got isl_bool = -1\")); }");
      method.line("};");
      Ok(())
    }
    CType::CString => {
      method.line(format!("let {} = unsafe {{ CStr::from_ptr({}) }};",
                          return_var, return_var));
      method.line(format!("let {} = {}.to_str().unwrap();", return_var, return_var));
      Ok(())
    }
    CType::ISLCtx
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
    | CType::ISLRestriction
    | CType::ISLVec
    | CType::ISLMat
    | CType::ISLPoint
    | CType::ISLVertex
    | CType::ISLCell
    | CType::ISLVertices
    | CType::ISLConstraint
    | CType::ISLConstraintList
    | CType::ISLStrideInfo
    | CType::ISLASTExpr
    | CType::ISLASTNode
    | CType::ISLASTNodeList
    | CType::ISLASTExprList
    | CType::ISLIdToASTExpr
    | CType::ISLFlow
    | CType::ISLUnionFlow
    | CType::ISLAccessInfo
    | CType::ISLUnionAccessInfo
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
    | CType::ISLScheduleConstraints => {
      let should_drop_on_free =
        if isl_func.ret_type == CType::ISLCtx && isl_func.name != "isl_ctx_alloc" {
          "false"
        } else {
          "true"
        };
      method.line(format!("let {} = {} {{ ptr: {}, should_free_on_drop: {} }};",
                          return_var,
                          get_rust_typename(isl_func.ret_type)?,
                          return_var,
                          should_drop_on_free));
      Ok(())
    }
    CType::Unsupported => bail!("Cannot convert this type to rust!"),
  }
}

pub fn generate_fn_bindings(scope: &mut Scope, type_: CType,
                            isl_functions: &HashSet<ISLFunction>)
                            -> Result<()> {
  let rust_ty_name = get_rust_typename(type_)?;
  let isl_typename = get_isl_struct_name(type_)?;
  let mut isl_functions_unsorted: Vec<&ISLFunction> =
    isl_functions.iter()
                 .filter(|f| f.has_all_known_types())
                 .filter(|f| f.parent_struct_type.is_some_and(|t| t == type_))
                 .collect();
  isl_functions_unsorted.sort_by_cached_key(|p| p.name.clone());
  let isl_functions = isl_functions_unsorted;

  // Import necessary types.
  for func in &isl_functions {
    for p in &func.parameters {
      if p.type_ != type_ {
        imports_for_type(p.type_, scope)?;
      }
    }
    if func.ret_type != type_ {
      imports_for_type(func.ret_type, scope)?;
    }
  }
  // Import LibISLError
  scope.import("super", "LibISLError");
  scope.import("super", "Error");

  // Define the struct for type_
  scope.new_struct(rust_ty_name)
       .field("pub ptr", "uintptr_t")
       .field("pub should_free_on_drop", "bool")
       .vis("pub")
       .doc(format!("Wraps `{}`.", isl_typename).as_str());

  // Declare extern functions
  scope.raw("extern \"C\" {");

  for func in &isl_functions {
    // TODO: Not ideal to emit raw strings, but `codegen` crate lacks support
    // for function declarations.
    // See https://gitlab.com/IovoslavIovchev/codegen/-/issues/11
    let args_str = func.parameters
                       .iter()
                       .map(|p| {
                         format!("{}: {}",
                                 p.name,
                                 get_typename_in_extern_block(p.type_).unwrap())
                       })
                       .collect::<Vec<String>>()
                       .join(", ");
    let ret_str = get_typename_in_extern_block(func.ret_type)?;
    scope.raw(format!("    fn {}({}) -> {};", func.name.clone(), args_str, ret_str));
  }
  scope.raw("}");

  // Define impl Struct
  let impl_scope = scope.new_impl(rust_ty_name);
  for func in &isl_functions {
    let method_name = guard_identifier(&func.name[isl_typename.len() + 1..].to_string());
    let mut method = impl_scope.new_fn(method_name.as_str())
                               .vis("pub")
                               .doc(format!("Wraps `{}`.", func.name).as_str());
    println!("Generating code for {}", func);

    let mut arg_names_in_fn_body: Vec<&str> = vec![];
    // Add self to the function's parameters (if needed.)
    if func.parameters.len() > 0 && func.parameters[0].type_ == type_ {
      match func.parameters[0].borrow {
        ISLBorrowRule::IslKeep => {
          method.arg_ref_self();
        }
        ISLBorrowRule::IslTake => {
          method.arg_self();
        }
        _ => bail!("Self can only be take or keep."),
      };
      arg_names_in_fn_body.push("self");
      method.line(format!("let {} = {};", func.parameters[0].name, "self"));
    }
    // Add rest of parameters to the binding function.
    for param in func.parameters[arg_names_in_fn_body.len()..].iter() {
      let borrow_str = match param.borrow {
                         ISLBorrowRule::IslTake | ISLBorrowRule::PassByValue => "",
                         ISLBorrowRule::IslKeep => "&",
                         _ => bail!("Encountered unsupported borrow rule."),
                       }.to_string();
      method.arg(param.name.as_str(),
                 borrow_str + get_rust_typename(param.type_)?);
    }

    // Grab the isl_context for error handling.
    let mut is_error_handling_possible = false;
    if type_ != CType::ISLCtx && method_name != "get_ctx" {
      for param in &func.parameters {
        if param.type_ == CType::ISLCtx {
          method.line(format!("let isl_rs_ctx = Context {{ ptr: {}.ptr, should_free_on_drop: false }};", param.name.clone()));
          is_error_handling_possible = true;
          break;
        }
        if is_isl_struct_type(param.type_) && param.type_ != CType::ISLOptions {
          method.line(format!("let isl_rs_ctx = {}.get_ctx();", param.name.clone()));
          is_error_handling_possible = true;
          break;
        }
      }
    }

    // Shadow vars before passing variable to libISL.
    for param in func.parameters.iter() {
      shadow_var_before_passing_to_isl_c(&mut method, param.type_, &param.name, param.borrow)?;
    }

    if is_error_handling_possible {
      method.ret(format!("Result<{}, LibISLError>", get_rust_typename(func.ret_type)?));
    } else {
      method.ret(get_rust_typename(func.ret_type)?);
    }

    let passed_args_str = func.parameters
                              .iter()
                              .map(|p| p.name.clone())
                              .collect::<Vec<String>>()
                              .join(", ");
    method.line(format!("let isl_rs_result = unsafe {{ {}({}) }};",
                        func.name, passed_args_str));
    // Shadow isl_rs_result.
    shadow_return_from_isl_c(&mut method, func, "isl_rs_result")?;

    // Error handling.
    if is_error_handling_possible {
      method.line("let err = isl_rs_ctx.last_error();");
      method.line("if err != Error::None_ {");
      method.line("let err_msg = isl_rs_ctx.last_error_msg();");
      method.line("isl_rs_ctx.reset_error();");
      method.line("return Err(LibISLError::new(err, err_msg));");
      method.line("}");
    }

    // Return isl_rs_result
    if is_error_handling_possible {
      method.line("Ok(isl_rs_result)");
    } else {
      method.line("isl_rs_result");
    }
  }
  impl_scope.new_fn("do_not_free_on_drop")
            .vis("pub")
            .doc(format!("Does not call {}_free() on being dropped. (For internal use only.)",
                         get_isl_struct_name(type_)?))
            .arg_mut_self()
            .line("self.should_free_on_drop = false;");

  // {{{ impl Drop for `type_`.

  let drop_impl = scope.new_impl(rust_ty_name);
  drop_impl.impl_trait("Drop");
  drop_impl.new_fn("drop")
           .arg_mut_self()
           .line("if self.should_free_on_drop {")
           .line(format!("    unsafe {{ {}_free(self.ptr); }}",
                         get_isl_struct_name(type_)?))
           .line("}");

  // }}}

  return Ok(());
}

pub fn generate_enums(scope: &mut Scope, enum_: &ISLEnum, variant_prefix_to_trim: &str)
                      -> Result<()> {
  let rust_ty_name = get_rust_typename(ctype_from_string(&enum_.name)?)?;
  let enum_def = scope.new_enum(rust_ty_name)
                      .vis("pub")
                      .derive("Debug")
                      .derive("Clone")
                      .derive("Copy")
                      .derive("PartialEq")
                      .derive("Eq");

  for variant in &enum_.variants {
    assert_eq!(variant[..variant_prefix_to_trim.len()],
               variant_prefix_to_trim.to_string());
    let variant_name_in_rust =
      guard_identifier(&variant[variant_prefix_to_trim.len()..].to_string()
                                                               .to_case(Case::Pascal));
    enum_def.new_variant(&variant_name_in_rust);
  }

  // {{{ Implement toi32

  let toi32 = scope.new_impl(rust_ty_name)
                   .new_fn("to_i32")
                   .vis("pub")
                   .doc(format!("Returns i32 values as defined in libisl.").as_str());
  toi32.line("match self {");
  for (variant, value) in enum_.variants.iter().zip(enum_.values.clone()) {
    assert_eq!(variant[..variant_prefix_to_trim.len()],
               variant_prefix_to_trim.to_string());
    let variant_name_in_rust =
      guard_identifier(&variant[variant_prefix_to_trim.len()..].to_string()
                                                               .to_case(Case::Pascal));
    toi32.line(format!("  {}::{} => {},", rust_ty_name, variant_name_in_rust, value));
  }
  toi32.arg_ref_self();
  toi32.ret("i32");
  toi32.line("}");

  // }}}

  // {{{ Implement fromi32

  let fromi32 =
    scope.new_impl(rust_ty_name)
         .new_fn("from_i32")
         .vis("pub")
         .doc(format!("Constructor based on the i32 values as defined in libisl.").as_str());
  fromi32.line("match val {");
  for (variant, value) in enum_.variants.iter().zip(enum_.values.clone()) {
    assert_eq!(variant[..variant_prefix_to_trim.len()],
               variant_prefix_to_trim.to_string());
    let variant_name_in_rust =
      guard_identifier(&variant[variant_prefix_to_trim.len()..].to_string()
                                                               .to_case(Case::Pascal));
    fromi32.line(format!("  {} => {}::{},", value, rust_ty_name, variant_name_in_rust));
  }

  fromi32.arg("val", "i32");
  fromi32.ret("Self");
  fromi32.line("  _ => panic!(\"Illegal value.\"),");
  fromi32.line("}");

  // }}}

  return Ok(());
}
