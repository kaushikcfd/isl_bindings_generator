// Copyright (c) 2022-2025 Kaushik Kulkarni
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

mod codegen;
mod cparse;
mod types;
mod utils;

use ::codegen::Scope;
use cparse::{extract_functions, ParseState};
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::path::Path;
use strum::IntoEnumIterator;

use crate::codegen::generate_enums;
use crate::codegen::generate_fn_bindings;
use crate::cparse::extract_enums;
use crate::types::ctype_from_string;
use crate::types::get_isl_struct_name;
use crate::types::get_rust_typename;
use crate::types::is_isl_struct_type;
use crate::types::CType;
use crate::types::ISLEnum;
use crate::types::ISLFunction;
use lazy_static::lazy_static;

lazy_static! {
  static ref ISL_HEADERS: &'static [&'static str] = &["space_type.h",
                                                      "ctx.h",
                                                      "options.h",
                                                      "polynomial_type.h",
                                                      "space.h",
                                                      "local_space.h",
                                                      "id.h",
                                                      "val.h",
                                                      "point.h",
                                                      "mat.h",
                                                      "vec.h",
                                                      "constraint.h",
                                                      "set.h",
                                                      "map.h",
                                                      "map_type.h",
                                                      "union_set.h",
                                                      "union_set_type.h",
                                                      "union_map.h",
                                                      "union_map_type.h",
                                                      "aff.h",
                                                      "polynomial.h",
                                                      "stride_info.h",
                                                      "fixed_box.h",
                                                      "printer.h",
                                                      "schedule_node.h",
                                                      "schedule.h",
                                                      "flow.h",
                                                      "lp.h",
                                                      "ilp.h",
                                                      "ast.h",
                                                      "vertices.h"];
  static ref ISL_ENUM_TO_PREFIX: HashMap<CType, &'static str> =
    HashMap::from([(CType::ISLDimType, "isl_dim_"),
                   (CType::ISLFold, "isl_fold_"),
                   (CType::ISLError, "isl_error_"),
                   (CType::ISLStat, "isl_stat_"),
                   (CType::ISLArgType, "isl_arg_"),
                   (CType::ISLScheduleNodeType, "isl_schedule_node_"),
                   (CType::ISLASTLoopType, "isl_ast_loop_")]);
}

fn isl_enums_parse_fallback(isl_enums: &mut HashSet<ISLEnum>) {
  isl_enums.insert(ISLEnum::new("isl_arg_type",
                                ["isl_arg_end",
                                 "isl_arg_alias",
                                 "isl_arg_arg",
                                 "isl_arg_bool",
                                 "isl_arg_child",
                                 "isl_arg_choice",
                                 "isl_arg_flags",
                                 "isl_arg_footer",
                                 "isl_arg_int",
                                 "isl_arg_user",
                                 "isl_arg_long",
                                 "isl_arg_ulong",
                                 "isl_arg_str",
                                 "isl_arg_str_list",
                                 "isl_arg_version"],
                                0..15));
  isl_enums.insert(ISLEnum::new("isl_schedule_node_type",
                                ["isl_schedule_node_error",
                                 "isl_schedule_node_band",
                                 "isl_schedule_node_context",
                                 "isl_schedule_node_domain",
                                 "isl_schedule_node_expansion",
                                 "isl_schedule_node_extension",
                                 "isl_schedule_node_filter",
                                 "isl_schedule_node_leaf",
                                 "isl_schedule_node_guard",
                                 "isl_schedule_node_mark",
                                 "isl_schedule_node_sequence",
                                 "isl_schedule_node_set"],
                                -1..11));
  isl_enums.insert(ISLEnum::new("isl_dim_type",
                                ["isl_dim_cst",
                                 "isl_dim_param",
                                 "isl_dim_in",
                                 "isl_dim_out",
                                 "isl_dim_set",
                                 "isl_dim_div",
                                 "isl_dim_all"],
                                [0, 1, 2, 3, 3, 4, 5]));
  isl_enums.insert(ISLEnum::new("isl_fold",
                                ["isl_fold_error",
                                 "isl_fold_min",
                                 "isl_fold_max",
                                 "isl_fold_list"],
                                -1..3));
  isl_enums.insert(ISLEnum::new("isl_error",
                                ["isl_error_none",
                                 "isl_error_abort",
                                 "isl_error_alloc",
                                 "isl_error_unknown",
                                 "isl_error_internal",
                                 "isl_error_invalid",
                                 "isl_error_quota",
                                 "isl_error_unsupported"],
                                0..8));
  isl_enums.insert(ISLEnum::new("isl_stat", ["isl_stat_error", "isl_stat_ok"], [-1, 0]));
  // isl_enums.insert(ISLEnum::new("isl_ast_expr_op_type",
  //                               ["isl_ast_expr_op_error",
  //                                "isl_ast_expr_op_and",
  //                                "isl_ast_expr_op_and_then",
  //                                "isl_ast_expr_op_or",
  //                                "isl_ast_expr_op_or_else",
  //                                "isl_ast_expr_op_max",
  //                                "isl_ast_expr_op_min",
  //                                "isl_ast_expr_op_minus",
  //                                "isl_ast_expr_op_add",
  //                                "isl_ast_expr_op_sub",
  //                                "isl_ast_expr_op_mul",
  //                                "isl_ast_expr_op_div",
  //                                "isl_ast_expr_op_fdiv_q",
  //                                "isl_ast_expr_op_pdiv_q",
  //                                "isl_ast_expr_op_pdiv_r",
  //                                "isl_ast_expr_op_zdiv_r",
  //                                "isl_ast_expr_op_cond",
  //                                "isl_ast_expr_op_select",
  //                                "isl_ast_expr_op_eq",
  //                                "isl_ast_expr_op_le",
  //                                "isl_ast_expr_op_lt",
  //                                "isl_ast_expr_op_ge",
  //                                "isl_ast_expr_op_gt",
  //                                "isl_ast_expr_op_call",
  //                                "isl_ast_expr_op_access",
  //                                "isl_ast_expr_op_member",
  //                                "isl_ast_expr_op_address_of"],
  //                               -1..26));
  // isl_enums.insert(ISLEnum::new("isl_ast_expr_type",
  //                               ["isl_ast_expr_error",
  //                                "isl_ast_expr_op",
  //                                "isl_ast_expr_id",
  //                                "isl_ast_expr_int"],
  //                               -1..3));
  // isl_enums.insert(ISLEnum::new("isl_ast_node_type",
  //                               ["isl_ast_node_error",
  //                                "isl_ast_node_for",
  //                                "isl_ast_node_if",
  //                                "isl_ast_node_block",
  //                                "isl_ast_node_mark",
  //                                "isl_ast_node_user"],
  //                               -1..5));
  isl_enums.insert(ISLEnum::new("isl_ast_loop_type",
                                ["isl_ast_loop_error",
                                 "isl_ast_loop_default",
                                 "isl_ast_loop_atomic",
                                 "isl_ast_loop_unroll",
                                 "isl_ast_loop_separate"],
                                -1..4));
}

pub fn main() {
  // {{{ Parsing

  let mut parse_state = ParseState { file_to_string: HashMap::new() };
  let mut isl_functions: HashSet<ISLFunction> = HashSet::new();
  let mut isl_enums: HashSet<ISLEnum> = HashSet::new();

  if false {
    // FIXME: There is a bug in the extract_enums where the values are not
    // correctly populated.
    for isl_header in ISL_HEADERS.iter() {
      isl_enums.extend(extract_enums(&(format!("isl/include/isl/{}", isl_header).to_string())).unwrap());
    }
  } else {
    isl_enums_parse_fallback(&mut isl_enums);
  }

  for isl_header in ISL_HEADERS.iter() {
    isl_functions.extend(extract_functions(&(format!("isl/include/isl/{}", isl_header).to_string()),
                                           &mut parse_state).unwrap());
  }

  // }}}

  // {{{ Codegen

  let mut mod_rs_scope = Scope::new();

  if Path::new("src/bindings/").is_dir() {
    fs::remove_dir_all("src/bindings/").expect("Removing `src/bindings` failed.");
  }
  fs::create_dir("src/bindings/").unwrap();

  for isl_type in CType::iter().filter(|t| is_isl_struct_type(t.clone())) {
    let isl_typename = get_isl_struct_name(isl_type).unwrap();
    let submodule_name = isl_typename[4..].to_string();
    let submodule_path = format!("{}/{}.rs", "src/bindings/", submodule_name);
    let rust_ty_name = get_rust_typename(isl_type).unwrap();

    let mut submodule_scope = Scope::new();
    generate_fn_bindings(&mut submodule_scope, isl_type, &isl_functions).unwrap();
    fs::write(
              &submodule_path,
              format!(
      "// Automatically generated by isl_bindings_generator.\n// LICENSE: MIT\n\n{}",
      submodule_scope.to_string()
    ),
    ).expect(format!("error writing to {} file.", submodule_path).as_str());

    mod_rs_scope.raw(format!("mod {};", submodule_name).as_str());
    mod_rs_scope.raw(format!("pub use {}::{};", submodule_name, rust_ty_name).as_str());
  }

  // }}}

  {
    let mut submodule_scope = Scope::new();
    let submodule_name = "enums";
    let submodule_path = format!("{}/{}.rs", "src/bindings/", submodule_name);
    mod_rs_scope.raw(format!("mod {};", submodule_name));
    for isl_enum in isl_enums {
      let rust_ty = ctype_from_string(&isl_enum.name).unwrap();
      let rust_ty_name = get_rust_typename(rust_ty).unwrap();
      generate_enums(&mut submodule_scope,
                     isl_enum,
                     ISL_ENUM_TO_PREFIX.get(&rust_ty).unwrap()).unwrap();

      fs::write(
                &submodule_path,
                format!(
        "// Automatically generated by isl_bindings_generator.\n// LICENSE: MIT\n\n{}",
        submodule_scope.to_string()
      ),
      ).expect(format!("error writing to {} file.", submodule_path).as_str());

      mod_rs_scope.raw(format!("pub use {}::{};", submodule_name, rust_ty_name).as_str());
    }
  }

  fs::write(
            "src/bindings/mod.rs",
            format!(
    "// Automatically generated by isl_bindings_generator.\n// LICENSE: MIT\n\n{}",
    mod_rs_scope.to_string()
  ),
  ).expect(format!("error writing to {} file.", "src/bindings/mod.rs").as_str());
}

// vim: fdm=marker
