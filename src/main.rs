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
use std::fs;
use std::path::Path;

pub fn main() {
  if Path::new("src/bindings/").is_dir() {
    fs::remove_dir_all("src/bindings/").expect("Removing `src/bindings` failed.");
  }
  fs::create_dir("src/bindings/").unwrap();

  let mut parse_state = ParseState { file_to_string: HashMap::new() };

  // extract_functions(&("isl/include/isl/ctx.h".to_string()), &mut parse_state).unwrap();
  extract_functions(&("isl/include/isl/val.h".to_string()), &mut parse_state).unwrap();
}
