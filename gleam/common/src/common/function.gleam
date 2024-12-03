/// Convert a binary function into a unary accepting a Tuple2 as input
pub fn uncurry2(passed_function: fn(a, b) -> c) -> fn(#(a, b)) -> c {
  fn(tuple) {
    let #(a, b) = tuple
    passed_function(a, b)
  }
}
