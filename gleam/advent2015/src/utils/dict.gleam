import gleam/dict.{type Dict}
import gleam/set.{type Set}

pub fn from_set(set: Set(#(a, b))) -> Dict(a, b) {
  set |> set.to_list() |> dict.from_list()
}
