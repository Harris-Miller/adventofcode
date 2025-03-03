import common/result as resultc
import gleam/int
import gleam/list.{index_map}
import gleam/option.{type Option}
import gleam/result

/// Return list without last value
pub fn init(list: List(a)) -> Result(List(a), Nil) {
  list
  |> list.reverse
  |> list.rest
  |> result.map(list.reverse)
}

/// Return list without last value. Panics if length < 1
pub fn init_(list: List(a)) -> List(a) {
  resultc.unwrap_or_panic(init(list), "init_ requires a list of length >= 1")
}

/// Find the minimum value of a List(Int)
pub fn minimum(list: List(Int)) -> Int {
  list.reduce(list, int.min) |> result.unwrap(0)
}

/// Find the maximum value of a List(Int)
pub fn maximum(list: List(Int)) -> Int {
  list.reduce(list, int.max) |> result.unwrap(0)
}

fn pairs_recurse(list: List(a)) -> List(#(a, a)) {
  case list {
    [a, b] -> [#(a, b)]
    [a, b, ..rest] -> [#(a, b), ..pairs_recurse([b, ..rest])]
    [_] | [] -> []
  }
}

/// Separate a list into pairs of tuples. If the list has length 0 or 1, `Error(Nil)` is returned.
///
/// ## Examples
/// 
/// ```gleam
/// pairs([1, 2, 3])
/// // -> Ok([#(1, 2), #(2, 3), #(2, 3)])
/// ```
/// 
/// ```gleam
/// pairs([])
/// // -> Error(Nil)
/// ```
///
pub fn pairs(list: List(a)) -> Result(List(#(a, a)), Nil) {
  case pairs_recurse(list) {
    [] -> Error(Nil)
    r -> Ok(r)
  }
}

/// Separate a list into pairs of tuples. If the list has length 0 or 1, this function will Panic
///
/// ## Examples
/// 
/// ```gleam
/// pairs([1, 2, 3])
/// // -> [#(1, 2), #(2, 3), #(2, 3)]
/// ```
/// 
/// ```gleam
/// pairs([])
/// // -> panic "pairs_ requires a list of length >= 2"
/// ```
///
pub fn pairs_(list: List(a)) -> List(#(a, a)) {
  resultc.unwrap_or_panic(pairs(list), "pairs_ requires a list of length >= 2")
}

/// Removes the index of a given list.
/// If index is outside of range, the original list will be returned
pub fn remove(list: List(a), at index: Int) -> Result(List(a), Nil) {
  let is_in_range = index >= 0 && index < list.length(list)
  let #(head, tail) = list.split(list, index)
  case is_in_range {
    False -> Error(Nil)
    True -> {
      tail |> list.rest() |> result.map(list.append(head, _))
    }
  }
}

pub fn remove_(list: List(a), at index: Int) -> List(a) {
  resultc.unwrap_or_panic(
    remove(list, index),
    "remove_ the received index \""
      <> int.to_string(index)
      <> "\" is out of range for list with length \""
      <> int.to_string(list.length(list))
      <> "\"",
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
  case list {
    [] -> consequence
    [head, ..tail] -> alternative(#(head, tail))
  }
}

pub fn with_index(list: List(a)) -> List(#(a, Int)) {
  index_map(list, fn(a, i) { #(a, i) })
}
