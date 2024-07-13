pub fn upwrap_assert(result: Result(a, b)) -> a {
  let assert Ok(a) = result
  a
}
