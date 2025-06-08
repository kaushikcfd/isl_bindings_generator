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

use std::collections::{HashMap, HashSet};

use anyhow::{bail, Result};
use codegen::{Function, Scope};

use crate::{
  types::{
    get_isl_struct_name, get_rust_typename, get_typename_in_extern_block, CType, ISLBorrowRule,
    ISLFunction,
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
    | CType::ISLDimType
    | CType::ISLError
    | CType::ISLFold
    | CType::ISLStat
    | CType::ISLScheduleConstaints => {
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
    | CType::ISLDimType
    | CType::ISLError
    | CType::ISLFold
    | CType::ISLStat
    | CType::ISLBool => {
      assert_eq!(borrow, ISLBorrowRule::IslKeep);
      Ok(())
    }
    CType::CString => {
      assert_eq!(borrow, ISLBorrowRule::IslKeep);
      method.line(format!("let {} = CString::new({}).unwrap();", arg_name, arg_name));
      method.line(format!("let {} = {}.as_ptr();", arg_name, arg_name));
      Ok(())
    }
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
    | CType::ISLScheduleConstaints => {
      match borrow {
        ISLBorrowRule::IslKeep => {
          method.line(format!("let mut {} = {};", arg_name, arg_name));
          method.line(format!("{}.do_not_free_on_drop();", arg_name));
          method.line(format!("let {} = {}.ptr;", arg_name, arg_name));
        }
        ISLBorrowRule::IslTake => {
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
    | CType::Sizet
    | CType::ISLDimType
    | CType::ISLError
    | CType::ISLFold
    | CType::ISLStat => Ok(()),
    CType::ISLBool => {
      method.line(format!("let {} = match {} {{", return_var, return_var));
      method.line("    0 => false,");
      method.line("    1 => true,");
      method.line("    _ => panic!(\"Got isl_bool = -1\"),");
      method.line("};");
      Ok(())
    }
    CType::CString => {
      method.line(format!("let {} = unsafe {{ CStr::from_ptr({}) }};",
                          return_var, return_var));
      method.line(format!("let {} = {}.to_str().unwrap();", return_var, return_var));
      Ok(())
    }
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
    | CType::ISLScheduleConstaints => {
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
  let isl_functions: Vec<&ISLFunction> = isl_functions.iter()
                                                      .filter(|f| f.has_all_known_types())
                                                      .filter(|f| f.name.starts_with(isl_typename))
                                                      .collect();

  // Import necessary types.
  for func in &isl_functions {
    for p in &func.parameters {
      imports_for_type(p.type_, scope)?;
    }
    imports_for_type(func.ret_type, scope)?;
  }

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
                       .map(|p| get_typename_in_extern_block(p.type_).unwrap().to_string())
                       .collect::<Vec<String>>()
                       .join(", ");
    let ret_str = get_typename_in_extern_block(func.ret_type)?;
    scope.raw(format!("    fn {}({}){};", func.name.clone(), args_str, ret_str));
  }
  scope.raw("}");

  // Define impl Struct
  let impl_scope = scope.new_impl(rust_ty_name);
  for func in &isl_functions {
    let method_name = guard_identifier(&func.name[isl_typename.len() + 1..].to_string());
    let mut method = impl_scope.new_fn(method_name.as_str())
                               .vis("pub")
                               .doc(format!("Wraps `{}`.", func.name).as_str());

    let mut arg_names_in_fn_body: Vec<&str> = vec![];
    if func.parameters[0].type_ == type_ {
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
    // Add parameters to the binding function.
    for param in func.parameters[arg_names_in_fn_body.len()..].iter() {
      let borrow_str = match param.borrow {
                         ISLBorrowRule::IslTake | ISLBorrowRule::PassByValue => "",
                         ISLBorrowRule::IslKeep => "&",
                         _ => bail!("Encountered unsupported borrow rule."),
                       }.to_string();
      method.arg(param.name.as_str(),
                 borrow_str + get_rust_typename(param.type_)?);
      shadow_var_before_passing_to_isl_c(&mut method, param.type_, &param.name, param.borrow)?;
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
    // Return isl_rs_result
    method.line("isl_rs_result");
  }

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
