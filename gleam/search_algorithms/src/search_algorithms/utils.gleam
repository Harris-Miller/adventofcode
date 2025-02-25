import gleam/bool
import gleam/list
import gleam/option.{type Option}
import gleam/result

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

pub fn uncons(list: List(a)) -> Option(#(a, List(a))) {
  case list {
    [head, ..tail] -> option.Some(#(head, tail))
    [] -> option.None
  }
}

pub fn uncons_guard(
  with list: List(a),
  return consequence: b,
  otherwise alternative: fn(#(a, List(a))) -> b,
) -> b {
  bool.guard(list.is_empty(list), consequence, fn() {
    let assert [head, ..tail] = list
    alternative(#(head, tail))
  })
}
