import gleam/list
import gleam/result

pub fn init(l: List(a)) -> Result(List(a), Nil) {
  l
  |> list.reverse
  |> list.rest
  |> result.map(list.reverse)
}
