import gleam/dict.{type Dict}
import gleam/option.{Some}
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
