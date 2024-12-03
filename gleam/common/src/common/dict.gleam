import gleam/dict.{type Dict}
import gleam/option.{None, Some}
import gleam/set.{type Set}

/// Convert a Set(#(a, b)) to a Dict(a, b)
pub fn from_set(set: Set(#(a, b))) -> Dict(a, b) {
  set |> set.to_list() |> dict.from_list()
}

/// Update an existing key in a Dict
/// If the key does not exist, the original Dict is returned
pub fn adjust(
  in dict: Dict(a, b),
  adjust key: a,
  with fun: fn(b) -> b,
) -> Dict(a, b) {
  case dict.has_key(dict, key) {
    False -> dict
    True -> {
      dict.upsert(dict, key, fn(rv) {
        let assert Some(v) = rv
        fun(v)
      })
    }
  }
}

/// Insert a new value into a Dict with an upsert function
/// If the key does not exist, the value is inserted directly
pub fn insert_with(
  in dict: Dict(a, b),
  adjust key: a,
  with value: b,
  using fun: fn(b, b) -> b,
) -> Dict(a, b) {
  dict.upsert(dict, key, fn(rv) {
    case rv {
      Some(v) -> fun(v, value)
      None -> value
    }
  })
}
