/// Unwrap a Result with assert, panics when `Error`.
pub fn unwrap_assert(result: Result(a, b)) -> a {
  let assert Ok(a) = result
  a
}

// Unwrap a Result, panic with specific message when `Error`
pub fn unwrap_or_panic(result: Result(a, b), message: String) -> a {
  case result {
    Ok(a) -> a
    Error(_) -> panic as message
  }
}
