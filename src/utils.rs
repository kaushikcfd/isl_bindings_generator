use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
  static ref KEYWORD_TO_IDEN: HashMap<&'static str, &'static str> =
    HashMap::from([("in", "in_"),
                   ("str", "str_"),
                   ("type", "type_"),
                   ("box", "box_"),
                   ("ref", "incref"),
                   ("mod", "mod_"),
                   ("2exp", "to_exp"),
                   ("None", "None_"),
                   ("match", "match_")]);
}
/// Returns an identifier based on `input` to avoid using a Rust-keyword.
pub fn guard_identifier<T: ToString>(input: &T) -> String {
  let input_str = input.to_string();
  match KEYWORD_TO_IDEN.get(input_str.as_str()) {
    Some(x) => x.to_string(),
    None => input.to_string(),
  }
}
