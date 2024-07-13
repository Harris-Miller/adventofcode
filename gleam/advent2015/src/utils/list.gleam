import gleam/int
import gleam/list
import gleam/result

pub fn init(list: List(a)) -> Result(List(a), Nil) {
  list
  |> list.reverse
  |> list.rest
  |> result.map(list.reverse)
}

pub fn minimum(list: List(Int)) -> Int {
  list.reduce(list, int.min) |> result.unwrap(0)
}

pub fn maximum(list: List(Int)) -> Int {
  list.reduce(list, int.max) |> result.unwrap(0)
}
