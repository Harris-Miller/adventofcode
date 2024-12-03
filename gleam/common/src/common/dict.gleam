import gleam/dict.{type Dict}
import gleam/option.{None, Some}
import gleam/set.{type Set}

pub fn from_set(set: Set(#(a, b))) -> Dict(a, b) {
  set |> set.to_list() |> dict.from_list()
}

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
