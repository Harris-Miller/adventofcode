import gleam/bool
import gleam/result

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

/// Combines result.unwrap with bool.guard
/// Useful for extracting out an Ok() and returning early with handling Error() into something completely different
pub fn unwrap_guard(
  with result: Result(a, b),
  return consequence: c,
  otherwise alternative: fn(a) -> c,
) -> c {
  bool.guard(result.is_error(result), consequence, fn() {
    let assert Ok(a) = result
    alternative(a)
  })
}

/// Combines result.unwrap with bool.guard
/// Useful for extracting out an Ok() and returning early with handling Error() into something completely different
pub fn unwrap_lazy_guard(
  with result: Result(a, b),
  return consequence: fn(b) -> c,
  otherwise alternative: fn(a) -> c,
) -> c {
  bool.lazy_guard(
    result.is_error(result),
    fn() {
      let assert Error(b) = result
      consequence(b)
    },
    fn() {
      let assert Ok(a) = result
      alternative(a)
    },
  )
}
