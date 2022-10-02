use clang;
use clang::token::{Token, TokenKind};
use codegen::Scope;
use hashbrown::{HashMap, HashSet};
use lazy_static::lazy_static;
use std::fs;
use std::iter::zip;
use std::option::Option;

lazy_static! {
    static ref C_TO_RS_BINDING: HashMap<&'static str, &'static str> =
        HashMap::from([("isl_ctx *", "Context"),
                       ("isl_space *", "Space"),
                       ("isl_local_space *", "LocalSpace"),
                       ("isl_id *", "Id"),
                       ("isl_multi_id *", "MultiId"),
                       ("isl_id_list *", "IdList"),
                       ("isl_val *", "Val"),
                       ("isl_multi_val *", "MultiVal"),
                       ("isl_point *", "Point"),
                       ("isl_mat *", "Mat"),
                       ("isl_basic_set *", "BasicSet"),
                       ("isl_basic_set_list *", "BasicSetList"),
                       ("isl_set *", "Set"),
                       ("isl_set_list *", "SetList"),
                       ("isl_basic_map *", "BasicMap"),
                       ("isl_map *", "Map"),
                       ("isl_aff *", "Aff"),
                       ("isl_pw_aff *", "PwAff"),
                       ("isl_multi_aff *", "MultiAff"),
                       ("isl_multi_pw_aff *", "MultiPwAff"),
                       ("isl_pw_multi_aff *", "PwMultiAff"),
                       ("isl_stride_info *", "StrideInfo"),
                       ("isl_fixed_box *", "FixedBox"),
                       ("enum isl_dim_type", "DimType")]);
    static ref ISL_CORE_TYPES: HashSet<&'static str> = HashSet::from(["isl_ctx *",
                                                                      "isl_space *",
                                                                      "isl_local_space *",
                                                                      "isl_id *",
                                                                      "isl_multi_id *",
                                                                      "isl_id_list *",
                                                                      "isl_val *",
                                                                      "isl_multi_val *",
                                                                      "isl_point *",
                                                                      "isl_mat *",
                                                                      "isl_basic_set *",
                                                                      "isl_basic_set_list *",
                                                                      "isl_set *",
                                                                      "isl_set_list *",
                                                                      "isl_basic_map *",
                                                                      "isl_map *",
                                                                      "isl_aff *",
                                                                      "isl_pw_aff *",
                                                                      "isl_multi_aff *",
                                                                      "isl_multi_pw_aff *",
                                                                      "isl_pw_multi_aff *",
                                                                      "isl_stride_info *",
                                                                      "isl_fixed_box *",]);
    static ref ISL_TYPES_RS: HashSet<&'static str> =
        HashSet::from_iter(C_TO_RS_BINDING.clone().into_values());
    static ref KEYWORD_TO_IDEN: HashMap<&'static str, &'static str> =
        HashMap::from([("in", "in_")]);

    // TODO: Once we reduce this set down to 0, we are done!
    static ref UNSUPPORTED_C_TYPES: HashSet<&'static str> =
        HashSet::from(["FILE *", "const FILE *",
                       "isl_set **",
                       "isl_val **",
                       "int *",
                       "isl_stat (*)(isl_basic_set *, void *)",
                       "isl_stat (*)(isl_point *, void *)",
                       "struct isl_options *",
                       "void *",
                       "char **",
                       "enum isl_error",
        ]);
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
    UNSUPPORTED_C_TYPES.contains(c_arg_t)
}

/// Returns the name for `c_arg_t` to use in `extern "C"` block function
/// declarations.
fn to_extern_arg_t(c_arg_t: String) -> String {
    let extern_t = if c_arg_t == "enum isl_dim_type" {
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
    } else if c_arg_t == "unsigned int" || c_arg_t == "uint32_t" {
        "u32"
    } else if c_arg_t == "unsigned long" {
        "u64"
    } else {
        panic!("Unexpected type: {}", c_arg_t)
    };

    extern_t.to_string()
}

/// Returns the name for `c_arg_t` to use in the rust-binding function.
fn to_rust_arg_t(c_arg_t: String, ownership: Option<ISLOwnership>) -> String {
    let c_arg_t = c_arg_t.as_str();
    if c_arg_t == "enum isl_dim_type" {
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
    } else if c_arg_t == "unsigned int" || c_arg_t == "uint32_t" {
        "u32".to_string()
    } else if c_arg_t == "unsigned long" {
        "u64".to_string()
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
        "i32" | "u32" | "bool" | "u64" => {}
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
        "i32" | "u32" | "bool" | "u64" | "DimType" => {}
        "&str" => {
            func.line(format!("let {} = CString::new({}).unwrap();", var_name, var_name));
            func.line(format!("let {} = {}.as_ptr();", var_name, var_name));
        }
        x if (ISL_TYPES_RS.contains(x)
              || (x.starts_with("&") && ISL_TYPES_RS.contains(&x[1..]))) =>
        {
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
                "i32" | "u32" | "u64" | "bool" | "DimType" => {}
                x if (ISL_TYPES_RS.contains(x)
                      || (x.starts_with("&") && ISL_TYPES_RS.contains(&x[1..]))) =>
                {
                    func.line(format!("let {} = {} {{ ptr: {} }};",
                                      var_name, rs_ty_name, var_name));
                }
                "&str" => {
                    func.line(format!("let {} = unsafe {{ CStr::from_ptr({}) }};",
                                      var_name, var_name));
                    func.line(format!("let {} = {}.to_str().unwrap();", var_name, var_name));
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
        println!("Traversing {}", func_decl.get_name().unwrap());
        let arguments = func_decl.get_arguments().unwrap();
        let (start_loc, end_loc) = get_start_end_locations(&func_decl);
        let (start_idx, end_idx) = (loc_to_idx[&start_loc], loc_to_idx[&end_loc]);
        assert!(start_idx <= end_idx);

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
                assert_eq!(qualifier2.get_kind(), TokenKind::Punctuation);
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
        let c_arg_names = arguments.iter()
                                   .map(|x| x.get_name().unwrap())
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
                      .arguments(&["-I", "isl/include/", "-I", "/usr/lib64/clang/13/include"])
                      .detailed_preprocessing_record(true)
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
                                       && e.get_location().is_some()
                                       && e.get_location().unwrap().get_presumed_location().0
                                          == src_file.to_string()
                                   })
                                   .collect();
    let (extern_funcs, binding_funcs) =
        get_extern_and_bindings_functions(func_decls, tokens, src_t);

    let mut scope = Scope::new();

    // {{{ Generate `use ...` statements.

    for func in extern_funcs.iter().chain(binding_funcs.iter()) {
        match &func.ret_type {
            Some(x) => import_type(&mut scope, x),
            None => {}
        };
        for arg_t in func.arg_types.iter() {
            import_type(&mut scope, arg_t);
        }
    }

    // }}}

    // {{{ Generate struct for dst_t

    scope.new_struct(dst_t).field("ptr", "uintptr_t").vis("pub");

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

        scope.raw(format!("    {}({}){};", extern_func.name, args_str, ret_str));
    }
    scope.raw("}");

    // }}}

    // {{{ Implement the struct 'dst_t'

    let dst_impl = scope.new_impl(dst_t);

    // KK: Assumption guarded by assertion. There is a one-to-one mapping between
    // the binding and the external functions
    assert_eq!(extern_funcs.len(), binding_funcs.len());

    for (extern_func, binding_func) in zip(extern_funcs, binding_funcs) {
        let mut impl_fn = dst_impl.new_fn(binding_func.name.as_str());
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

        postprocess_var_from_extern_func(&mut impl_fn, binding_func.ret_type, "isl_rs_result");

        impl_fn.line("isl_rs_result");
    }

    // }}}

    // {{{ impl Drop for `dst_t`.

    let drop_impl = scope.new_impl(dst_t);
    drop_impl.impl_trait("Drop");
    drop_impl.new_fn("drop")
             .arg_mut_self()
             .line(format!("unsafe {{ {}_free(self.ptr) }}", src_t));

    // }}}

    // Write the generated code
    fs::write(dst_file, scope.to_string()).expect(format!("error writing to {} file.", dst_file)
                                                  .as_str());
}

/// Generate rust code to define the `isl_dim_type` enum declation in rust and
/// writes the generated to the `dst_file` path. (Touches the file if not
/// already present)
///
/// # Warnings
///  
/// - Overwrites the contents of `dst_file`.
fn define_dim_type_enum(dst_file: &str, src_file: &str) {
    let clang = clang::Clang::new().unwrap();
    let index = clang::Index::new(&clang, false, true);
    let t_unit = index.parser(src_file)
                      .arguments(&["-I", "isl/include/", "-I", "/usr/lib64/clang/13/include"])
                      .detailed_preprocessing_record(true)
                      .parse()
                      .unwrap();

    let isl_dim_type_decl = t_unit.get_entity()
                                  .get_children()
                                  .into_iter()
                                  .filter(|e| {
                                      e.get_kind() == clang::EntityKind::EnumDecl
                                      && e.get_display_name().is_some()
                                      && e.get_display_name().unwrap() == "isl_dim_type"
                                  })
                                  .next()
                                  .unwrap();

    // KK: Assertion to guard assumption
    assert!(isl_dim_type_decl.get_children()
                             .into_iter()
                             .all(|x| x.get_kind() == clang::EntityKind::EnumConstantDecl));

    let c_variant_names = isl_dim_type_decl.get_children()
                                           .into_iter()
                                           .map(|x| x.get_display_name().unwrap())
                                           .collect::<Vec<_>>();

    // KK: Assertion to guard assumption
    assert!(c_variant_names.iter().all(|x| x.starts_with("isl_dim_")));

    let mut scope = Scope::new();
    let dim_type_enum = scope.new_enum(C_TO_RS_BINDING["enum isl_dim_type"])
                             .repr("C")
                             .derive("Display")
                             .derive("Debug")
                             .derive("Clone");
    for c_variant_name in c_variant_names {
        let name_in_rust = c_variant_name[8..].to_string();
        dim_type_enum.new_variant(guard_identifier(name_in_rust));
    }

    // Write the generated code
    fs::write(dst_file, scope.to_string()).expect("error writing to dim_type file");
}

fn main() {
    fs::remove_dir_all("src/bindings/").expect("Removing `src/bindings` failed.");
    fs::create_dir("src/bindings/").unwrap();

    define_dim_type_enum("src/bindings/dim_type.rs", "isl/include/isl/space_type.h");

    implement_bindings("Context",
                       "isl_ctx",
                       "src/bindings/context.rs",
                       "isl/include/isl/ctx.h");
    implement_bindings("BasicSet",
                       "isl_basic_set",
                       "src/bindings/bset.rs",
                       "isl/include/isl/set.h");
    implement_bindings("Set",
                       "isl_set",
                       "src/bindings/set.rs",
                       "isl/include/isl/set.h");
}

// vim:fdm=marker
