pub fn assert_unwrap(result: Result(a, b)) -> a {
  let assert Ok(a) = result
  a
}
