use clang;
use clang::token::{Token, TokenKind};
use codegen::Scope;
use hashbrown::{HashMap, HashSet};
use lazy_static::lazy_static;
use std::fs;
use std::option::Option;
use std::path::Path;

lazy_static! {
    static ref C_TO_RS_EXTERN: HashMap<&'static str, &'static str> =
        HashMap::from([("isl_context *", "uintptr_t"),
                       ("isl_basic_set *", "uintptr_t"),
                       ("isl_basic_map *", "uintptr_t"),
                       ("isl_set *", "uintptr_t"),
                       ("isl_map *", "uintptr_t"),
                       ("isl_aff *", "uintptr_t"),
                       ("const char*", "*const c_char"),]);
    static ref C_TO_RS_BINDING: HashMap<&'static str, &'static str> =
        HashMap::from([("isl_basic_set *", "BasicSet"),
                       ("isl_context *", "Context"),
                       ("isl_aff *", "Aff"),
                       ("enum isl_dim_type", "DimType"),
                       ("const char*", "&str"),]);
    static ref ISL_TYPES: HashSet<&'static str> = HashSet::from(["isl_aff",
                                                                 "isl_context",
                                                                 "isl_basic_set",
                                                                 "isl_set",
                                                                 "isl_basic_map",
                                                                 "isl_map"]);
}

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

fn get_start_end_locations(e: clang::Entity) -> ((usize, usize), (usize, usize)) {
    let src_range = e.get_range().unwrap();
    let start_src_loc = src_range.get_start().get_presumed_location();
    let end_src_loc = src_range.get_end().get_presumed_location();
    ((start_src_loc.1 as usize, start_src_loc.2 as usize),
     (end_src_loc.1 as usize, end_src_loc.2 as usize))
}

struct Function {
    name: String,
    arg_names: Vec<String>,
    arg_types: Vec<String>,
    ret_type: Option<String>,
}

#[derive(Debug)]
enum ISLOwnership {
    Keep,
    Take,
}

fn get_extern_and_bindings_functions(func_decls: Vec<clang::Entity>, tokens: Vec<Token>)
                                     -> (Vec<Function>, Vec<Function>) {
    // external_functions: External functions that must be declared.
    let external_functions: Vec<Function> = vec![];
    // bindings_functions: Rust functions that are to be generated.
    let bindings_functions: Vec<Function> = vec![];
    let (loc_to_idx, idx_to_token) = get_tokens_sorted_by_occurence(tokens);

    for func_decl in func_decls {
        println!("Traversing {}", func_decl.get_name().unwrap());
        let (start_loc, end_loc) = get_start_end_locations(func_decl);
        let (start_idx, end_idx) = (loc_to_idx[&start_loc], loc_to_idx[&end_loc]);
        assert!(start_idx <= end_idx);

        // {{{ parse __isl_null, __isl_give

        let (isl_null, isl_give) =
            if idx_to_token[start_idx - 1].get_kind() == TokenKind::Punctuation {
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

        let (is_constructor, is_exported) = if isl_give {
            let qualifier1 = idx_to_token[start_idx - 2];
            let qualifier2 = idx_to_token[start_idx - 3];
            if qualifier1.get_kind() == TokenKind::Punctuation {
                (false, false)
            } else if qualifier1.get_spelling() == "__isl_export" {
                assert_eq!(qualifier2.get_kind(), TokenKind::Punctuation);
                (false, true)
            } else {
                assert_eq!(qualifier1.get_spelling(), "__isl_constructor");
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
            } else {
                assert_eq!(qualifier.get_spelling(), "__isl_export");
                assert!((idx_to_token[start_idx - 2].get_kind() == TokenKind::Punctuation)
                        | (idx_to_token[start_idx - 2].get_spelling() == "endif"));
                (false, true)
            }
        };

        // }}}

        let arguments = func_decl.get_arguments().unwrap();
        let c_arg_types = arguments.iter()
                                   .map(|x| x.get_type().unwrap().get_display_name())
                                   .collect::<Vec<_>>();
        let mut borrowing_rules: Vec<Option<ISLOwnership>> = vec![];
        for arg in arguments {
            let (start_loc, _) = get_start_end_locations(arg);
            let qualifier_tok = idx_to_token[loc_to_idx[&start_loc] - 1];
            let borrow_rule = if qualifier_tok.get_kind() == TokenKind::Identifier {
                match qualifier_tok.get_spelling().as_str() {
                    "__isl_take" => Some(ISLOwnership::Take),
                    "__isl_keep" => Some(ISLOwnership::Keep),
                    x => panic!("Unknown ownership rule {}", x),
                }
            } else {
                assert_eq!(qualifier_tok.get_kind(), TokenKind::Punctuation);
                None
            };

            borrowing_rules.push(borrow_rule);
        }

        println!("{:?}", c_arg_types);
        println!("{:?}", borrowing_rules);

        panic!("Abhi key liye bas bhai");
    }

    (external_functions, bindings_functions)
}

fn implement_bindings(dst_t: &str, src_t: &str, _dst_file: &str, src_file: &str) {
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
    let (extern_funcs, binding_funcs) = get_extern_and_bindings_functions(func_decls, tokens);

    let mut scope = Scope::new();

    // {{{ Generate struct for dst_t

    scope.import("libc", "uintptr_t");
    scope.new_struct(dst_t).field("ptr", "uintptr_t");

    // }}}

    // {{{ TODO: Implement the struct 'dst_t'

    let _dst_impl = scope.new_impl(dst_t);

    // }}}

    // {{{ impl Drop for `dst_t`.

    let drop_impl = scope.new_impl(dst_t);
    drop_impl.impl_trait("Drop");
    drop_impl.new_fn("drop")
             .arg_mut_self()
             .line(format!("unsafe {{ {}_free(self.ptr) }}", src_t));

    // }}}

    panic!("The code generated is --\n{}", scope.to_string());
}

fn main() {
    if !Path::new("src/bindings/").is_dir() {
        fs::create_dir("src/bindings/").unwrap();
    }

    implement_bindings("BasicSet",
                       "isl_basic_set",
                       "src/bindings/bset.rs",
                       "isl/include/isl/set.h");
}

// vim:fdm=marker
