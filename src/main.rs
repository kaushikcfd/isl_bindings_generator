use clang;
use clang::token::Token;
use codegen::Scope;
use hashbrown::HashMap;
use lazy_static::lazy_static;
use std::fs;
use std::path::Path;

lazy_static! {
    static ref C_TO_RS_BINDING: HashMap<&'static str, &'static str> =
        HashMap::from([("isl_basic_set", "BasicSet"),
                       ("isl_context", "Context"),
                       ("isl_aff", "Aff"),
                       ("isl_dim_type", "DimType"),
                       ("const char*", "&str"),]);
    static ref C_TO_RS_EXTERN: HashMap<&'static str, &'static str> =
        HashMap::from([("isl_context", "uintptr_t"),
                       ("isl_basic_set", "uintptr_t"),
                       ("isl_basic_map", "uintptr_t"),
                       ("isl_set", "uintptr_t"),
                       ("isl_map", "uintptr_t"),
                       ("isl_aff", "uintptr_t"),
                       ("const char*", "*const c_char"),]);
}

fn compare_tuples(x: &(u32, u32), y: &(u32, u32)) -> std::cmp::Ordering {
    if x.0 == y.0 && x.1 == y.1 {
        std::cmp::Ordering::Equal
    } else if (x.0 < y.0) || (x.0 == y.0 && x.1 < y.1) {
        std::cmp::Ordering::Less
    } else {
        std::cmp::Ordering::Greater
    }
}

fn get_tokens_sorted_by_occurence(tokens: Vec<Token>) -> (HashMap<(u32, u32), u32>, Vec<Token>) {
    let mut loc_to_token: HashMap<(u32, u32), Token> = HashMap::new();
    for token in tokens {
        let loc = token.get_location();
        let (_, src_line, src_column) = loc.get_presumed_location();
        let key = (src_line, src_column);
        loc_to_token.insert(key, token);
    }

    let mut sorted_locations: Vec<(u32, u32)> = loc_to_token.clone().into_keys().collect();
    sorted_locations.sort_by(compare_tuples);

    let position_to_token: Vec<_> = sorted_locations.iter().map(|x| loc_to_token[x]).collect();
    let mut loc_to_position: HashMap<(u32, u32), u32> = HashMap::new();
    for (i, loc) in sorted_locations.into_iter().enumerate() {
        loc_to_position.insert(loc, i as u32);
    }

    (loc_to_position, position_to_token)
}

fn get_start_end_locations(e: clang::Entity) -> ((u32, u32), (u32, u32)) {
    let src_range = e.get_range().unwrap();
    let start_src_loc = src_range.get_start().get_presumed_location();
    let end_src_loc = src_range.get_end().get_presumed_location();
    ((start_src_loc.1, start_src_loc.2), (end_src_loc.1, end_src_loc.2))
}

fn implement_bindings(dst_t: &str, src_t: &str, _dst_file: &str, src_file: &str) {
    let clang = clang::Clang::new().unwrap();
    let index = clang::Index::new(&clang, false, true);
    let t_unit = index.parser(src_file)
                      .arguments(&["-I", "isl/include/", "-I", "/usr/lib64/clang/13/include"])
                      .detailed_preprocessing_record(true)
                      .parse()
                      .unwrap();

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

    let (loc_to_idx, idx_to_token) =
        get_tokens_sorted_by_occurence(t_unit.get_entity().get_range().unwrap().tokenize());

    let mut scope = Scope::new();
    scope.import("libc", "uintptr_t");
    scope.new_struct(dst_t).field("ptr", "uintptr_t");
    let _dst_impl = scope.new_impl(dst_t);

    for func_decl in func_decls {
        println!("{:#?}", func_decl);
        let (start_loc, end_loc) = get_start_end_locations(func_decl);
        let (start_idx, end_idx) = (loc_to_idx[&start_loc], loc_to_idx[&end_loc]);

        assert!(start_idx <= end_idx);

        for idx in start_idx - 1..(end_idx + 1) {
            println!("{:#?}", idx_to_token[idx as usize]);
        }

        // for child in func_decl.get_children() {
        //     println!("{:#?}", child);
        // }
        // let parm_decls = func_decl.get_arguments().unwrap();
        // for parm_decl in parm_decls {
        //     println!("Parameter.get_type {:#?}", parm_decl.get_type());
        //     println!("Parameter.get_name {:#?}", parm_decl.get_name());
        // }
        panic!("Dandey aur kaam baaki hai");
    }

    // {{{ impl Drop for `dst_t`.

    let drop_impl = scope.new_impl(dst_t);
    drop_impl.impl_trait("Drop");
    drop_impl.new_fn("drop")
             .arg_mut_self()
             .line(format!("unsafe {{ {}_free(self.ptr) }}", { src_t }));

    // }}}
    //
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
