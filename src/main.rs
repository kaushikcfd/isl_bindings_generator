use hashbrown::HashMap;
use lazy_static::lazy_static;
use codegen::Scope;
use clang;
use std::path::Path;
use std::fs;

lazy_static! {
    static ref C_TO_RS_BINDING: HashMap<&'static str, &'static str> = HashMap::from([
        ("isl_basic_set", "BasicSet"),
        ("isl_context", "Context"),
        ("isl_aff", "Aff"),
        ("isl_dim_type", "DimType"),
        ("const char*", "&str"),
    ]);

    static ref C_TO_RS_EXTERN: HashMap<&'static str, &'static str> = HashMap::from([
        ("isl_context", "uintptr_t"),
        ("isl_basic_set", "uintptr_t"),
        ("isl_basic_map", "uintptr_t"),
        ("isl_set", "uintptr_t"),
        ("isl_map", "uintptr_t"),
        ("isl_aff", "uintptr_t"),
        ("const char*", "*const c_char"),
    ]);
}


fn implement_bindings(dst_t: &str, src_t: &str, _dst_file: &str, src_file: &str) {
    let clang = clang::Clang::new().unwrap();
    let index = clang::Index::new(&clang, false, true);
    let t_unit = index.parser(src_file)
                       .arguments(&["-I", "isl/include/", "-I", "/usr/lib64/clang/13/include"])
                       .parse().unwrap();
    let func_decls: Vec<_> = t_unit.get_entity().get_children().into_iter().filter(|e| {
        e.get_kind() == clang::EntityKind::FunctionDecl
            && e.get_name().is_some()
            && e.get_name().unwrap().starts_with(src_t)
            && e.get_location().is_some()
            && e.get_location().unwrap().get_presumed_location().0 == src_file.to_string()
    }).collect();

    let mut scope = Scope::new();
    scope.import("libc", "uintptr_t");
    scope.new_struct(dst_t)
         .field("ptr", "uintptr_t");
    let _dst_impl = scope.new_impl(dst_t);

    for func_decl in func_decls {
        let parm_decls = func_decl.get_arguments().unwrap();
        for parm_decl in parm_decls {
            println!("Parameter.get_type {:#?}",
                     parm_decl.get_type());
            println!("Parameter.get_name {:#?}",
                     parm_decl.get_name());
        }
        panic!("Dandey aur kaam baaki hai");
    }

    // {{{ impl Drop for `dst_t`.

    let drop_impl = scope.new_impl(dst_t);
    drop_impl.impl_trait("Drop");
    drop_impl.new_fn("drop")
             .arg_mut_self()
             .line(format!("unsafe {{ {}_free(self.ptr) }}", {src_t}));

    // }}}
    //
    panic!("The code generated is --\n{}", scope.to_string());
}

fn main() {
    if !Path::new("src/bindings/").is_dir() {
        fs::create_dir("src/bindings/").unwrap();
    }

    implement_bindings("BasicSet", "isl_basic_set", "src/bindings/bset.rs", "isl/include/isl/set.h");
}
