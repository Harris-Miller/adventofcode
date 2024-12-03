pub fn uncurry2(passed_function: fn(a, b) -> c) -> fn(#(a, b)) -> c {
  fn(tuple) {
    let #(a, b) = tuple
    passed_function(a, b)
  }
}
