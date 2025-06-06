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

mod cparse;
mod types;

use cparse::{extract_functions, ParseState};
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::path::Path;

use crate::cparse::extract_enums;
use crate::types::ISLEnum;
use crate::types::ISLFunction;
use lazy_static::lazy_static;

lazy_static! {
  static ref ISL_HEADERS: &'static [&'static str] = &["space_type.h",
                                                      "ctx.h",
                                                      "polynomial_type.h",
                                                      "space.h",
                                                      "local_space.h",
                                                      "id.h",
                                                      "val.h",
                                                      "point.h",
                                                      "mat.h",
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
                                                      "schedule_type.h",
                                                      "schedule.h",];
}

pub fn main() {
  if Path::new("src/bindings/").is_dir() {
    fs::remove_dir_all("src/bindings/").expect("Removing `src/bindings` failed.");
  }
  fs::create_dir("src/bindings/").unwrap();

  let mut parse_state = ParseState { file_to_string: HashMap::new() };
  let mut isl_functions: HashSet<ISLFunction> = HashSet::new();
  let mut isl_enums: HashSet<ISLEnum> = HashSet::new();

  for isl_header in ISL_HEADERS.iter() {
    isl_enums.extend(extract_enums(&(format!("isl/include/isl/{}", isl_header).to_string()),
                                   &mut parse_state).unwrap());
  }
  if true {
    panic!("Hello World");
  }

  for isl_header in ISL_HEADERS.iter() {
    isl_functions.extend(extract_functions(&(format!("isl/include/isl/{}", isl_header).to_string()),
                                           &mut parse_state).unwrap());
  }

  println!("#ISL functions = {}.", isl_functions.len());
}
