// Copyright (c) 2022 Kaushik Kulkarni
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

use clang;
use clang::token::{Token, TokenKind};
use codegen::Scope;
use hashbrown::{HashMap, HashSet};
use lazy_static::lazy_static;
use std::fs;
use std::iter::zip;
use std::option::Option;
use std::path::Path;

static CLANG_ARGS: [&'static str; 4] = ["-I",
                                        "isl/include/",
                                        "-I",
                                        "/usr/lib/llvm-19/lib/clang/19/include/"];

lazy_static! {

    static ref C_TO_RS_BINDING: HashMap<&'static str, &'static str> =
        HashMap::from([("isl_ctx *", "Context"),
                       ("isl_space *", "Space"),
                       ("isl_local_space *", "LocalSpace"),
                       ("isl_id *", "Id"),
                       ("isl_val *", "Val"),
                       ("isl_point *", "Point"),
                       ("isl_mat *", "Mat"),
                       ("isl_vec *", "Vec"),
                       ("isl_basic_set *", "BasicSet"),
                       ("isl_constraint *", "Constraint"),
                       ("isl_set *", "Set"),
                       ("isl_term *", "Term"),
                       ("isl_basic_map *", "BasicMap"),
                       ("isl_map *", "Map"),
                       ("isl_aff *", "Aff"),
                       ("isl_pw_aff *", "PwAff"),
                       ("isl_schedule *", "Schedule"),
                       ("isl_schedule_constraints *", "ScheduleConstraints"),
                       ("isl_schedule_node *", "ScheduleNode"),
                       ("isl_qpolynomial *", "QPolynomial"),
                       ("isl_pw_qpolynomial *", "PwQPolynomial"),
                       ("isl_qpolynomial_fold *", "QPolynomialFold"),
                       ("isl_pw_qpolynomial_fold *", "PwQPolynomialFold"),
                       ("isl_stride_info *", "StrideInfo"),
                       ("isl_fixed_box *", "FixedBox"),
                       ("enum isl_dim_type", "DimType"),
                       ("enum isl_fold", "Fold"),
                       ("enum isl_error", "Error"),
                       ("enum isl_schedule_node_type", "ScheduleNodeType")
                       ]);
   static ref ISL_CORE_TYPES: HashSet<&'static str> = C_TO_RS_BINDING.keys().filter(|k| !k.starts_with("enum isl_")).copied().collect();
   static ref ISL_TYPES_RS: HashSet<&'static str> = C_TO_RS_BINDING.values().copied().collect();
   static ref KEYWORD_TO_IDEN: HashMap<&'static str, &'static str> =
        HashMap::from([("in", "in_"),
                       ("str", "str_"),
                       ("type", "type_"),
                       ("box", "box_"),
                       ("ref", "incref"),
                       ("mod", "mod_"),
                       ("2exp", "to_exp"),
                       ("match", "match_")]);

    // TODO: Once we reduce this set down to 0, we are done!
    static ref UNSUPPORTED_C_TYPES: HashSet<&'static str> =
        HashSet::from(["FILE *", "const FILE *",
                       "isl_set **",
                       "isl_val **",
                       "int *",
                       "isl_bool *",
                       "isl_stat (*)(isl_basic_set *, void *)",
                       "isl_stat (*)(isl_point *, void *)",
                       "isl_options *",
                       "void *",
                       "const void *",
                       "void (*)(void *)",
                       "char **",
                       "isl_mat **",
                       "isl_id_list *",
                       "isl_basic_set_list *",
                       "isl_basic_map_list *",
                       "isl_set_list *",
                       "isl_map_list *",
                       "isl_aff_list *",
                       "isl_union_pw_aff *",
                       "isl_multi_aff *",
                       "isl_multi_pw_aff *",
                       "isl_pw_multi_aff *",
                       "isl_multi_val *",
                       "isl_multi_id *",
                       "isl_union_pw_qpolynomial *",
                       "isl_union_set *",
                       "isl_union_map *",
                       "isl_multi_union_pw_aff *",
                       "isl_multi_union_pw_multi_aff *",
                       "isl_union_pw_multi_aff *",
                       "isl_union_pw_qpolynomial_fold *",
                       "isl_pw_qpolynomial_fold_list *",
                       "isl_qpolynomial **",
        ]);

    // TODO: Once we reduce this set down to 0, we are done!
    static ref UNSUPPORTED_FUNCS: HashSet<&'static str> =
        HashSet::from([]);
}

/// Returns the lexicographic ordering of `x` and `y`.
fn compare_tuples(x: &(usize, usize), y: &(usize, usize)) -> std::cmp::Ordering {
    if x.0 == y.0 && x.1 == y.1 {
        std::cmp::Ordering::Equal
    } else if (x.0 < y.0) || (x.0 == y.0 && x.1 < y.1) {
        std::cmp::Ordering::Less
    } else {
        std::cmp::Ordering::Greater
    }
}

fn get_tokens_sorted_by_occurence(tokens: Vec<Token>)
                                  -> (HashMap<(usize, usize), usize>, Vec<Token>) {
    let mut loc_to_token: HashMap<(usize, usize), Token> = HashMap::new();
    for token in tokens {
        let loc = token.get_location();
        let (_, src_line, src_column) = loc.get_presumed_location();
        let key = (src_line as usize, src_column as usize);
        loc_to_token.insert(key, token);
    }

    let mut sorted_locations: Vec<(usize, usize)> = loc_to_token.clone().into_keys().collect();
    sorted_locations.sort_by(compare_tuples);

    let position_to_token: Vec<_> = sorted_locations.iter().map(|x| loc_to_token[x]).collect();
    let mut loc_to_position: HashMap<(usize, usize), usize> = HashMap::new();
    for (i, loc) in sorted_locations.into_iter().enumerate() {
        loc_to_position.insert(loc, i as usize);
    }

    (loc_to_position, position_to_token)
}

/// Returns the `(start_line, start_column), (end_line, end_column)` describing
/// source range of `e`.
fn get_start_end_locations(e: &clang::Entity) -> ((usize, usize), (usize, usize)) {
    let src_range = e.get_range().unwrap();
    let start_src_loc = src_range.get_start().get_presumed_location();
    let end_src_loc = src_range.get_end().get_presumed_location();
    ((start_src_loc.1 as usize, start_src_loc.2 as usize),
     (end_src_loc.1 as usize, end_src_loc.2 as usize))
}

/// Records the properties of a function node in an AST.
#[derive(Clone)]
struct Function {
    /// name of the function symbol
    name: String,
    /// Argument names
    arg_names: Vec<String>,
    /// Argument types
    arg_types: Vec<String>,
    /// Return type
    ret_type: Option<String>,
}

#[derive(Debug, Copy, Clone)]
enum ISLOwnership {
    Keep,
    Take,
}

/// Returns an identifier based on `input` to avoid using a Rust-keyword.
fn guard_identifier(input: impl ToString) -> String {
    let input_str = input.to_string();
    match KEYWORD_TO_IDEN.get(input_str.as_str()) {
        Some(x) => x.to_string(),
        None => input.to_string(),
    }
}

/// Returns `true` only if `c_arg_t` is reference to a core isl object.
/// Note that we do not consider `isl_dim_type` to be a core isl object.
fn is_isl_type(c_arg_t: &impl ToString) -> bool {
    let c_arg_t = &c_arg_t.to_string()[..];
    if ISL_CORE_TYPES.contains(c_arg_t) {
        true
    } else if c_arg_t.starts_with("const ")
              && ISL_CORE_TYPES.contains(c_arg_t[6..].to_string().as_str())
    {
        true
    } else if c_arg_t.starts_with("struct ")
              && ISL_CORE_TYPES.contains(c_arg_t[7..].to_string().as_str())
    {
        true
    } else {
        false
    }
}

/// Returns `true` only if `c_arg_t` is a type not supported by
/// [`isl_bindings_generator`].
fn is_type_not_supported(c_arg_t: &String) -> bool {
    let c_arg_t = &c_arg_t[..];
    let new_c_arg_t = if c_arg_t.starts_with("struct ") {
        c_arg_t[7..].to_string()
    } else {
        c_arg_t.to_string()
    };

    UNSUPPORTED_C_TYPES.contains(new_c_arg_t.as_str())
}

/// Returns the name for `c_arg_t` to use in `extern "C"` block function
/// declarations.
fn to_extern_arg_t(c_arg_t: String) -> String {
    let extern_t = if c_arg_t == "enum isl_dim_type"
                      || c_arg_t == "enum isl_fold"
                      || c_arg_t == "enum isl_error"
                      || c_arg_t == "enum isl_schedule_node_type"
    {
        C_TO_RS_BINDING[c_arg_t.as_str()]
    } else if is_isl_type(&c_arg_t) {
        "uintptr_t"
    } else if c_arg_t == "isl_size" {
        // FIXME: Add add an assertion for this assumption.
        // KK: Assumption: `# typedef isl_size i32`
        "i32"
    } else if c_arg_t == "isl_bool" {
        // Using i32 for isl_bool as it is not a real type.
        // Will panic for -1
        "i32"
    } else if c_arg_t == "const char *" {
        "*const c_char"
    } else if c_arg_t == "char *" {
        "*const c_char"
    } else if c_arg_t == "int" {
        "i32"
    } else if c_arg_t == "long" {
        "i64"
    } else if c_arg_t == "unsigned int" || c_arg_t == "uint32_t" {
        "u32"
    } else if c_arg_t == "unsigned long" {
        "u64"
    } else if c_arg_t == "size_t" {
        "usize"
    } else if c_arg_t == "double" {
        "f64"
    } else {
        panic!("Unexpected type: {}", c_arg_t)
    };

    extern_t.to_string()
}

/// Returns the name for `c_arg_t` to use in the rust-binding function.
fn to_rust_arg_t(c_arg_t: String, ownership: Option<ISLOwnership>) -> String {
    let c_arg_t = c_arg_t.as_str();
    if c_arg_t == "enum isl_dim_type"
       || c_arg_t == "enum isl_fold"
       || c_arg_t == "enum isl_error"
       || c_arg_t == "enum isl_schedule_node_type"
    {
        C_TO_RS_BINDING[c_arg_t].to_string()
    } else if is_isl_type(&c_arg_t) {
        let c_arg_t = if c_arg_t.starts_with("const ") {
            c_arg_t[6..].to_string()
        } else if c_arg_t.starts_with("struct ") {
            c_arg_t[7..].to_string()
        } else {
            c_arg_t.to_string()
        };
        let c_arg_t = c_arg_t.as_str();
        match ownership.unwrap() {
            ISLOwnership::Keep => format!("&{}", C_TO_RS_BINDING[c_arg_t]),
            ISLOwnership::Take => C_TO_RS_BINDING[c_arg_t].to_string(),
        }
    } else if c_arg_t == "isl_size" {
        // FIXME: Add add an assertion for this assumption.
        // KK: Assumption: `# typedef isl_size i32`
        "i32".to_string()
    } else if c_arg_t == "isl_bool" {
        // isl_bool_error should be panic-ed.
        "bool".to_string()
    } else if c_arg_t == "const char *" {
        "&str".to_string()
    } else if c_arg_t == "char *" {
        "&str".to_string()
    } else if c_arg_t == "int" {
        "i32".to_string()
    } else if c_arg_t == "long" {
        "i64".to_string()
    } else if c_arg_t == "unsigned int" || c_arg_t == "uint32_t" {
        "u32".to_string()
    } else if c_arg_t == "unsigned long" {
        "u64".to_string()
    } else if c_arg_t == "size_t" {
        "usize".to_string()
    } else if c_arg_t == "double" {
        "f64".to_string()
    } else {
        panic!("Unexpected type: {}", c_arg_t)
    }
}

/// Imports `ty_name` from the correct path for `scope`.
fn import_type(scope: &mut Scope, ty_name: &String) {
    let ty_name = ty_name.as_str();

    match ty_name {
        "uintptr_t" => {
            scope.import("libc", "uintptr_t");
        }
        "i32" | "u32" | "bool" | "u64" | "i64" | "f64" | "usize" => {}
        "&str" => {
            scope.import("std::ffi", "CString");
            scope.import("std::ffi", "CStr");
        }
        "*const c_char" => {
            scope.import("std::os::raw", "c_char");
        }
        x if ISL_TYPES_RS.contains(x) => {
            scope.import("crate::bindings", x);
        }
        x if x.starts_with("&") && ISL_TYPES_RS.contains(&x[1..]) => {
            scope.import("crate::bindings", &x[1..]);
        }

        _ => panic!("Unknown type '{}'.", ty_name),
    };
}

/// Updates `func` by adding a line shadowing the variable `var_name` to pass it
/// legally to an external function.
fn preprocess_var_to_extern_func(func: &mut codegen::Function, rs_ty_name: &String,
                                 var_name: impl ToString) {
    let rs_ty_name = rs_ty_name.as_str();
    let var_name = var_name.to_string();

    match rs_ty_name {
        "i32" | "u32" | "bool" | "u64" | "i64" | "f64" | "usize" | "DimType" | "Fold" | "Error"
        | "ScheduleNodeType" => {}
        "&str" => {
            func.line(format!("let {} = CString::new({}).unwrap();", var_name, var_name));
            func.line(format!("let {} = {}.as_ptr();", var_name, var_name));
        }
        x if ISL_TYPES_RS.contains(x) => {
            func.line(format!("let mut {} = {};", var_name, var_name));
            func.line(format!("{}.do_not_free_on_drop();", var_name));
            func.line(format!("let {} = {}.ptr;", var_name, var_name));
        }
        x if (x.starts_with("&") && ISL_TYPES_RS.contains(&x[1..])) => {
            func.line(format!("let {} = {}.ptr;", var_name, var_name));
        }
        _ => unimplemented!("{}", rs_ty_name),
    };
}

/// Updates `func` by adding a line shadowing the variable `var_name` to refer
/// it's corresponding type in Rust land.
fn postprocess_var_from_extern_func(func: &mut codegen::Function, rs_ty_name: Option<String>,
                                    var_name: impl ToString) {
    match rs_ty_name {
        Some(rs_ty_name) => {
            let var_name = var_name.to_string();

            match rs_ty_name.as_str() {
                "i32" | "u32" | "u64" | "i64" | "f64" | "usize" | "DimType" | "Fold" | "Error"
                | "ScheduleNodeType" => {}
                x if (ISL_TYPES_RS.contains(x)
                      || (x.starts_with("&") && ISL_TYPES_RS.contains(&x[1..]))) =>
                {
                    func.line(format!("let {} = {} {{ ptr: {}, should_free_on_drop: true }};",
                                      var_name, rs_ty_name, var_name));
                }
                "&str" => {
                    func.line(format!("let {} = unsafe {{ CStr::from_ptr({}) }};",
                                      var_name, var_name));
                    func.line(format!("let {} = {}.to_str().unwrap();", var_name, var_name));
                }
                "bool" => {
                    func.line(format!("let {} = match {} {{", var_name, var_name));
                    func.line("    0 => false,");
                    func.line("    1 => true,");
                    func.line("    _ => panic!(\"Got isl_bool = -1\"),");
                    func.line("};");
                }
                _ => unimplemented!("{}", rs_ty_name),
            };
        }
        None => {
            // Function does not return anything.
        }
    };
}

/// Returns the method name for the binding to generate on the Rust end.
fn get_rust_method_name(func_decl: &clang::Entity, c_struct_t: &str) -> String {
    let c_name = func_decl.get_name().unwrap();
    // Remove the type prefix (For eg. isl_basic_set_read_from_str -> read_from_str)
    let name_in_rust = c_name[c_struct_t.len() + 1..].to_string();
    guard_identifier(name_in_rust)
}

fn get_extern_and_bindings_functions(func_decls: Vec<clang::Entity>, tokens: Vec<Token>,
                                     src_t: &str)
                                     -> (Vec<Function>, Vec<Function>) {
    // external_functions: External functions that must be declared.
    let mut external_functions: Vec<Function> = vec![];
    // bindings_functions: Rust functions that are to be generated.
    let mut bindings_functions: Vec<Function> = vec![];
    let (loc_to_idx, idx_to_token) = get_tokens_sorted_by_occurence(tokens);

    for func_decl in func_decls {
        // println!("Traversing {}", func_decl.get_name().unwrap());
        let arguments = func_decl.get_arguments().unwrap();
        let (start_loc, _) = get_start_end_locations(&func_decl);
        let start_idx = loc_to_idx[&start_loc];

        // {{{ parse __isl_null, __isl_give

        let (isl_null, isl_give) =
            if idx_to_token[start_idx - 1].get_kind() == TokenKind::Punctuation {
                (false, false)
            } else if idx_to_token[start_idx - 1].get_kind() == TokenKind::Comment {
                (false, false)
            } else {
                let qualifier_1 = idx_to_token[start_idx - 1];
                assert_eq!(qualifier_1.get_kind(), TokenKind::Identifier);
                if qualifier_1.get_spelling() == "__isl_null" {
                    assert!(idx_to_token[start_idx - 2].get_spelling() != "__isl_give");
                    (true, false)
                } else if qualifier_1.get_spelling() == "__isl_give" {
                    assert!(idx_to_token[start_idx - 2].get_spelling() != "__isl_null");
                    (false, true)
                } else {
                    (false, false)
                }
            };

        // }}}

        // {{{ parse __isl_export, __isl_constructor

        let (_is_constructor, _is_exported) = if isl_give {
            let qualifier1 = idx_to_token[start_idx - 2];
            let qualifier2 = idx_to_token[start_idx - 3];
            if qualifier1.get_kind() == TokenKind::Punctuation {
                (false, false)
            } else if qualifier1.get_spelling() == "__isl_export" {
                assert_eq!(qualifier2.get_kind(), TokenKind::Punctuation);
                (false, true)
            } else {
                assert!(qualifier1.get_spelling() == "__isl_constructor"
                        || qualifier1.get_spelling() == "__isl_overload");
                assert!(qualifier2.get_kind() == TokenKind::Punctuation
                        || qualifier2.get_spelling() == "endif");
                (true, false)
            }
        } else if isl_null {
            let qualifier = idx_to_token[start_idx - 2];
            if qualifier.get_kind() == TokenKind::Punctuation {
                (false, false)
            } else {
                assert_eq!(qualifier.get_spelling(), "__isl_export");
                (false, true)
            }
        } else {
            let qualifier = idx_to_token[start_idx - 1];
            if qualifier.get_kind() == TokenKind::Punctuation {
                (false, false)
            } else if qualifier.get_spelling() == "endif"
                      || qualifier.get_spelling() == "__isl_keep"
            {
                (false, false)
            } else if qualifier.get_kind() == TokenKind::Comment {
                (false, false)
            } else {
                assert_eq!(qualifier.get_spelling(), "__isl_export");
                assert!((idx_to_token[start_idx - 2].get_kind() == TokenKind::Punctuation)
                        | (idx_to_token[start_idx - 2].get_spelling() == "endif"));
                (false, true)
            }
        };

        // }}}

        // {{{ parse borrowing_rules

        let mut borrowing_rules: Vec<Option<ISLOwnership>> = vec![];
        for arg in arguments.iter() {
            let (start_loc, _) = get_start_end_locations(&arg);
            let qualifier_tok = idx_to_token[loc_to_idx[&start_loc] - 1];
            let borrow_rule = if qualifier_tok.get_kind() == TokenKind::Identifier {
                match qualifier_tok.get_spelling().as_str() {
                    "__isl_take" => Some(ISLOwnership::Take),
                    "__isl_keep" => Some(ISLOwnership::Keep),
                    "__isl_give" => None, // FIXME
                    x => panic!("Unknown ownership rule {}", x),
                }
            } else if arg.get_type().unwrap().get_display_name() == "isl_ctx *"
                      || arg.get_type().unwrap().get_display_name() == "struct isl_ctx *"
            {
                // isl_ctx is always kept
                Some(ISLOwnership::Keep)
            } else if func_decl.get_name().unwrap().ends_with("_copy") {
                Some(ISLOwnership::Keep)
            } else {
                assert_eq!(qualifier_tok.get_kind(), TokenKind::Punctuation);
                None
            };

            borrowing_rules.push(borrow_rule);
        }

        // }}}

        let c_arg_types = arguments.iter()
                                   .map(|x| x.get_type().unwrap().get_display_name())
                                   .collect::<Vec<_>>();
        let ret_type = func_decl.get_result_type()
                                .map(|x| x.get_display_name())
                                .filter(|x| x != "void");

        if c_arg_types.iter().any(|x| is_type_not_supported(x))
           || ret_type.clone()
                      .map_or(false, |x| is_type_not_supported(&x))
        {
            println!("SKIPPPING {}", func_decl.get_name().unwrap());
            continue;
        }
        let c_arg_names =
            arguments.iter()
                     .map(|x| x.get_name().unwrap())
                     .map(|x| KEYWORD_TO_IDEN.get(x.as_str()).map_or(x, |y| y.to_string()))
                     .collect::<Vec<_>>();

        let extern_func = Function { name: func_decl.get_name().unwrap(),
                                     arg_names: c_arg_names.clone(),
                                     arg_types: c_arg_types.clone()
                                                           .into_iter()
                                                           .map(|x| to_extern_arg_t(x))
                                                           .collect(),
                                     ret_type: ret_type.clone().map(|x| to_extern_arg_t(x)) };

        let binding_func =
            Function { name: get_rust_method_name(&func_decl, src_t),
                       arg_names: c_arg_names,
                       arg_types:
                           zip(c_arg_types, borrowing_rules).into_iter()
                                                            .map(|(x, brw)| to_rust_arg_t(x, brw))
                                                            .collect(),
                       ret_type: ret_type.map(|x| to_rust_arg_t(x, Some(ISLOwnership::Take))) };

        external_functions.push(extern_func);
        bindings_functions.push(binding_func);
    }

    (external_functions, bindings_functions)
}

/// Generates Rust bindings for type `dst_t` from the C-struct `src_t`. Searches
/// for functions within `src_file` and the generated code is written to
/// `dst_file`.
fn implement_bindings(dst_t: &str, src_t: &str, dst_file: &str, src_file: &str) {
    let clang = clang::Clang::new().unwrap();
    let index = clang::Index::new(&clang, false, true);
    let t_unit = index.parser(src_file)
                      .arguments(&CLANG_ARGS)
                      .parse()
                      .unwrap();
    let tokens = t_unit.get_entity().get_range().unwrap().tokenize();

    // func_decls: Functions for which bindings are to be generated
    let func_decls: Vec<_> = t_unit.get_entity()
                                   .get_children()
                                   .into_iter()
                                   .filter(|e| {
                                       e.get_kind() == clang::EntityKind::FunctionDecl
                                       && e.get_name().is_some()
                                       && e.get_name().unwrap().starts_with(src_t)
                                       // match isl_set, but not isl_set_list
                                       && ! e.get_name().unwrap().starts_with(format!("{}_list", src_t).as_str())
                                       // FIXME: to_list functions have unfavorable tokens
                                       && e.get_name().unwrap() != format!("{}_to_list", src_t)
                                       && ! UNSUPPORTED_FUNCS.contains(e.get_name().unwrap().as_str())
                                       && e.get_location().is_some()
                                       && e.get_location().unwrap().get_presumed_location().0
                                          == src_file.to_string()
                                   })
                                   .collect();
    let (extern_funcs, binding_funcs) =
        get_extern_and_bindings_functions(func_decls, tokens, src_t);

    let mut scope = Scope::new();

    // {{{ Generate `use ...` statements.

    // Always use uintptr_t as dst_t's struct requires it.
    import_type(&mut scope, &"uintptr_t".to_string());
    for func in extern_funcs.iter().chain(binding_funcs.iter()) {
        match &func.ret_type {
            Some(x) if x != dst_t && &x[1..] != dst_t => import_type(&mut scope, x),
            _ => {}
        };
        for arg_t in func.arg_types.iter() {
            if arg_t != dst_t && &arg_t[1..] != dst_t {
                import_type(&mut scope, arg_t);
            }
        }
    }

    // }}}

    // {{{ Generate struct for dst_t

    scope.new_struct(dst_t)
         .field("pub ptr", "uintptr_t")
         .field("pub should_free_on_drop", "bool")
         .vis("pub")
         .doc(format!("Wraps `{}`.", src_t).as_str());

    // }}}

    // {{{ Declare the extern functions

    scope.raw("extern \"C\" {");
    for extern_func in extern_funcs.clone() {
        // TODO: Not ideal to emit raw strings, but `codegen` crate lacks support for
        // function declarations.
        // See https://gitlab.com/IovoslavIovchev/codegen/-/issues/11
        let args_str = zip(extern_func.arg_names, extern_func.arg_types).map(|(name, ty)| {
                                                                            format!("{}: {}",
                                                                                    name, ty)
                                                                        })
                                                                        .collect::<Vec<String>>()
                                                                        .join(", ");
        let ret_str = extern_func.ret_type
                                 .map_or("".to_string(), |x| format!(" -> {}", x));

        scope.raw(format!("    fn {}({}){};", extern_func.name, args_str, ret_str));
    }
    scope.raw("}");

    // }}}

    // {{{ Implement the struct 'dst_t'

    let dst_impl = scope.new_impl(dst_t);

    // KK: Assumption guarded by assertion. There is a one-to-one mapping between
    // the binding and the external functions
    assert_eq!(extern_funcs.len(), binding_funcs.len());

    for (extern_func, binding_func) in zip(extern_funcs, binding_funcs) {
        let mut impl_fn = dst_impl.new_fn(binding_func.name.as_str())
                                  .vis("pub")
                                  .doc(format!("Wraps `{}`.", extern_func.name).as_str());
        // FIXME: /!\ Big FIXME. This logic doesn't account
        let mut bnd_arg_names: Vec<String> = binding_func.arg_names.clone();
        let mut bnd_arg_types: Vec<String> = binding_func.arg_types.clone();
        let mut arg_names_in_fn_body: Vec<String> = vec![];

        // emit first argument to the method
        if bnd_arg_types.len() != 0 && bnd_arg_types[0] == format!("&{}", dst_t) {
            // consume the first argument
            impl_fn = impl_fn.arg_ref_self();
            bnd_arg_names = bnd_arg_names[1..].to_vec();
            bnd_arg_types = bnd_arg_types[1..].to_vec();
            arg_names_in_fn_body.push("self".to_string());
        } else if bnd_arg_types.len() != 0 && bnd_arg_types[0] == dst_t {
            // consume the first argument
            impl_fn = impl_fn.arg_self();
            bnd_arg_names = bnd_arg_names[1..].to_vec();
            bnd_arg_types = bnd_arg_types[1..].to_vec();
            arg_names_in_fn_body.push("self".to_string());
        } else {
            // do nothing
        }

        // add the rest of the arguments
        for (arg_name, arg_t) in zip(bnd_arg_names.iter(), bnd_arg_types.iter()) {
            impl_fn = impl_fn.arg(arg_name, arg_t);
            arg_names_in_fn_body.push(arg_name.to_string());
        }

        // add the return type
        match binding_func.ret_type.clone() {
            Some(x) => impl_fn.ret(x),
            None => impl_fn,
        };

        // Implement the function
        for (arg_type, (arg_name, arg_name_in_fn_body)) in zip(binding_func.arg_types.iter(),
                                                               zip(binding_func.arg_names.iter(),
                                                                   arg_names_in_fn_body.iter()))
        {
            if arg_name != arg_name_in_fn_body {
                impl_fn.line(format!("let {} = {};", arg_name, arg_name_in_fn_body));
            }

            preprocess_var_to_extern_func(&mut impl_fn, arg_type, arg_name);
        }

        let passed_args_str = binding_func.arg_names.join(", ");

        impl_fn.line(format!("let isl_rs_result = unsafe {{ {}({}) }};",
                             extern_func.name, passed_args_str));

        postprocess_var_from_extern_func(&mut impl_fn,
                                         binding_func.ret_type.clone(),
                                         "isl_rs_result");

        // {{{ Do not free isl_ctx* if not from isl_ctx_alloc.

        match binding_func.ret_type {
            Some(x)
                if (x == C_TO_RS_BINDING["isl_ctx *"] && extern_func.name != "isl_ctx_alloc") =>
            {
                impl_fn.line("let mut isl_rs_result = isl_rs_result;");
                impl_fn.line("isl_rs_result.do_not_free_on_drop();");
            }
            _ => {}
        };

        // }}}

        impl_fn.line("isl_rs_result");
    }

    dst_impl.new_fn("do_not_free_on_drop")
            .vis("pub")
            .doc("Does not call isl_xxx_free() on being dropped. (For internal use only.)")
            .arg_mut_self()
            .line("self.should_free_on_drop = false;");

    // }}}

    // {{{ impl Drop for `dst_t`.

    let drop_impl = scope.new_impl(dst_t);
    drop_impl.impl_trait("Drop");
    drop_impl.new_fn("drop")
             .arg_mut_self()
             .line("if self.should_free_on_drop {")
             .line(format!("    unsafe {{ {}_free(self.ptr); }}", src_t))
             .line("}");

    // }}}

    // Write the generated code
    fs::write(
              dst_file,
              format!(
        "// Automatically generated by isl_bindings_generator.\n// LICENSE: MIT\n\n{}",
        scope.to_string()
    ),
    ).expect(format!("error writing to {} file.", dst_file).as_str());
}

/// Generate rust code to define enum declation in rust corresponding to `src_t`
/// and writes the generated to the `dst_file` path. (Touches the file if not
/// already present)
///
/// # Warnings
///  
/// - Overwrites the contents of `dst_file`.
fn define_enum(src_t: &str, variant_prefixes: &str, dst_file: &str, src_file: &str) {
    let clang = clang::Clang::new().unwrap();
    let index = clang::Index::new(&clang, false, true);
    let t_unit = index.parser(src_file)
                      .arguments(&CLANG_ARGS)
                      .detailed_preprocessing_record(true)
                      .parse()
                      .unwrap();

    let isl_enum_decl = t_unit.get_entity()
                              .get_children()
                              .into_iter()
                              .filter(|e| {
                                  e.get_kind() == clang::EntityKind::EnumDecl
                                  && e.get_display_name().is_some()
                                  && e.get_display_name().unwrap() == src_t
                              })
                              .next()
                              .unwrap();

    // KK: Assertion to guard assumption
    assert!(isl_enum_decl.get_children()
                         .into_iter()
                         .all(|x| x.get_kind() == clang::EntityKind::EnumConstantDecl));

    let c_variant_names = isl_enum_decl.get_children()
                                       .into_iter()
                                       .map(|x| x.get_display_name().unwrap())
                                       .collect::<Vec<_>>();

    // KK: Assertion to guard assumption
    assert!(c_variant_names.iter()
                           .all(|x| x.starts_with(variant_prefixes)));

    let mut scope = Scope::new();
    let rust_enum = scope.new_enum(C_TO_RS_BINDING[format!("enum {}", src_t).as_str()])
                         .vis("pub")
                         .repr("C")
                         .derive("Debug")
                         .derive("Clone");
    for c_variant_name in c_variant_names {
        let name_in_rust = c_variant_name[variant_prefixes.len()..].to_string(); // convert variant name to camel case
        let name_in_rust = format!("{}{}",
                                   &name_in_rust[..1].to_uppercase(),
                                   &name_in_rust[1..]);
        rust_enum.new_variant(guard_identifier(name_in_rust));
    }

    // Write the generated code
    fs::write(
              dst_file,
              format!(
        "// Automatically generated by isl_bindings_generator.\n// LICENSE: MIT\n{}",
        scope.to_string()
    ),
    ).expect("error writing to dim_type file");
}

/// Populates `src/bindings/mod.rs` with isl types.
fn generate_bindings_mod(dst_file: &str) {
    let mut scope = Scope::new();

    scope.raw("mod dim_type;");
    scope.raw("mod fold;");
    scope.raw("mod fixed_box;");
    scope.raw("mod stride_info;");
    scope.raw("mod context;");
    scope.raw("mod space;");
    scope.raw("mod local_space;");
    scope.raw("mod id;");
    // scope.raw("mod multi_id;");
    scope.raw("mod val;");
    // scope.raw("mod multi_val;");
    scope.raw("mod point;");
    scope.raw("mod mat;");
    scope.raw("mod vec;");
    scope.raw("mod bset;");
    scope.raw("mod set;");
    scope.raw("mod bmap;");
    scope.raw("mod map;");
    scope.raw("mod aff;");
    scope.raw("mod pw_aff;");
    scope.raw("mod term;");
    scope.raw("mod constraint;");
    scope.raw("mod qpolynomial;");
    scope.raw("mod pw_qpolynomial;");
    scope.raw("mod qpolynomial_fold;");
    scope.raw("mod pw_qpolynomial_fold;");

    scope.raw("pub use dim_type::DimType;");
    scope.raw("pub use fold::Fold;");
    scope.raw("pub use error::Error;");
    scope.raw("pub use schedule_node_type::ScheduleNodeType;");
    scope.raw("pub use fixed_box::FixedBox;");
    scope.raw("pub use stride_info::StrideInfo;");

    scope.raw("pub use context::Context;");
    scope.raw("pub use space::Space;");
    scope.raw("pub use local_space::LocalSpace;");
    scope.raw("pub use id::Id;");
    // scope.raw("pub use multi_id::MultiId;");
    scope.raw("pub use val::Val;");
    // scope.raw("pub use multi_val::MultiVal;");
    scope.raw("pub use point::Point;");
    scope.raw("pub use mat::Mat;");
    scope.raw("pub use vec::Vec;");
    scope.raw("pub use bset::BasicSet;");
    scope.raw("pub use set::Set;");
    scope.raw("pub use bmap::BasicMap;");
    scope.raw("pub use map::Map;");
    scope.raw("pub use aff::Aff;");
    scope.raw("pub use pw_aff::PwAff;");
    scope.raw("pub use term::Term;");
    scope.raw("pub use constraint::Constraint;");
    scope.raw("pub use qpolynomial::QPolynomial;");
    scope.raw("pub use pw_qpolynomial::PwQPolynomial;");
    scope.raw("pub use qpolynomial_fold::QPolynomialFold;");
    scope.raw("pub use pw_qpolynomial_fold::PwQPolynomialFold;");
    scope.raw("pub use schedule_constraints::ScheduleConstraints;");
    scope.raw("pub use schedule_node::ScheduleNode;");
    scope.raw("pub use schedule::Schedule;");

    // Write the generated code
    fs::write(dst_file, scope.to_string()).expect("error writing to dim_type file");
}

fn main() {
    if Path::new("src/bindings/").is_dir() {
        fs::remove_dir_all("src/bindings/").expect("Removing `src/bindings` failed.");
    }

    fs::create_dir("src/bindings/").unwrap();

    define_enum("isl_dim_type",
                "isl_dim_",
                "src/bindings/dim_type.rs",
                "isl/include/isl/space_type.h");
    define_enum("isl_fold",
                "isl_fold_",
                "src/bindings/fold.rs",
                "isl/include/isl/polynomial_type.h");
    define_enum("isl_error",
                "isl_error_",
                "src/bindings/error.rs",
                "isl/include/isl/ctx.h");
    define_enum("isl_schedule_node_type",
                "isl_schedule_node_",
                "src/bindings/schedule_node_type.rs",
                "isl/include/isl/schedule_type.h");

    // {{{ emit bindings for primitive types

    implement_bindings("Context",
                       "isl_ctx",
                       "src/bindings/context.rs",
                       "isl/include/isl/ctx.h");
    implement_bindings("Space",
                       "isl_space",
                       "src/bindings/space.rs",
                       "isl/include/isl/space.h");
    implement_bindings("LocalSpace",
                       "isl_local_space",
                       "src/bindings/local_space.rs",
                       "isl/include/isl/local_space.h");
    implement_bindings("Id", "isl_id", "src/bindings/id.rs", "isl/include/isl/id.h");
    implement_bindings("Val",
                       "isl_val",
                       "src/bindings/val.rs",
                       "isl/include/isl/val.h");
    implement_bindings("Point",
                       "isl_point",
                       "src/bindings/point.rs",
                       "isl/include/isl/point.h");
    implement_bindings("Mat",
                       "isl_mat",
                       "src/bindings/mat.rs",
                       "isl/include/isl/mat.h");
    implement_bindings("Vec",
                       "isl_vec",
                       "src/bindings/vec.rs",
                       "isl/include/isl/vec.h");
    implement_bindings("BasicSet",
                       "isl_basic_set",
                       "src/bindings/bset.rs",
                       "isl/include/isl/set.h");
    implement_bindings("Constraint",
                       "isl_constraint",
                       "src/bindings/constraint.rs",
                       "isl/include/isl/constraint.h");
    implement_bindings("Set",
                       "isl_set",
                       "src/bindings/set.rs",
                       "isl/include/isl/set.h");
    implement_bindings("BasicMap",
                       "isl_basic_map",
                       "src/bindings/bmap.rs",
                       "isl/include/isl/map.h");
    implement_bindings("Map",
                       "isl_map",
                       "src/bindings/map.rs",
                       "isl/include/isl/map.h");
    implement_bindings("Aff",
                       "isl_aff",
                       "src/bindings/aff.rs",
                       "isl/include/isl/aff.h");
    implement_bindings("PwAff",
                       "isl_pw_aff",
                       "src/bindings/pw_aff.rs",
                       "isl/include/isl/aff.h");
    implement_bindings("Term",
                       "isl_term",
                       "src/bindings/term.rs",
                       "isl/include/isl/polynomial.h");
    implement_bindings("QPolynomial",
                       "isl_qpolynomial",
                       "src/bindings/qpolynomial.rs",
                       "isl/include/isl/polynomial.h");
    implement_bindings("QPolynomialFold",
                       "isl_qpolynomial_fold",
                       "src/bindings/qpolynomial_fold.rs",
                       "isl/include/isl/polynomial.h");
    implement_bindings("PwQPolynomial",
                       "isl_pw_qpolynomial",
                       "src/bindings/pw_qpolynomial.rs",
                       "isl/include/isl/polynomial.h");
    implement_bindings("PwQPolynomialFold",
                       "isl_pw_qpolynomial_fold",
                       "src/bindings/pw_qpolynomial_fold.rs",
                       "isl/include/isl/polynomial.h");
    implement_bindings("StrideInfo",
                       "isl_stride_info",
                       "src/bindings/stride_info.rs",
                       "isl/include/isl/stride_info.h");
    implement_bindings("FixedBox",
                       "isl_fixed_box",
                       "src/bindings/fixed_box.rs",
                       "isl/include/isl/fixed_box.h");
    implement_bindings("ScheduleConstraints",
                       "isl_schedule_constraints",
                       "src/bindings/schedule_constraints.rs",
                       "isl/include/isl/schedule.h");
    implement_bindings("ScheduleNode",
                       "isl_schedule_node",
                       "src/bindings/schedule_node.rs",
                       "isl/include/isl/schedule.h");
    implement_bindings("Schedule",
                       "isl_schedule",
                       "src/bindings/schedule.rs",
                       "isl/include/isl/schedule.h");

    // }}}

    // add `uses` to src/bindings/mod.rs
    generate_bindings_mod("src/bindings/mod.rs");
}

// vim:fdm=marker
