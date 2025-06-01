pub enum CType {
  Int,
}

pub enum ISLBorrowRule {
  IslKeep,
  IslTake,
  PassByValue,
}

pub struct Parameter {
  name: String,
  borrow: ISLBorrowRule,
}

pub struct ISLFunction {
  name: String,
  parameters: Vec<Parameter>,
  ret_type: CType,
}
