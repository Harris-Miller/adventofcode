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

pub fn pairs(list: List(a)) -> List(#(a, a)) {
  case list {
    [a, b] -> [#(a, b)]
    [a, b, ..rest] -> [#(a, b), ..pairs([b, ..rest])]
    [_] | [] -> panic as "pairs requires a list of at least length==2"
  }
}

pub fn remove(list: List(a), at index: Int) -> List(a) {
  let #(head, tail) = list.split(list, index)
  case tail {
    [] -> head
    _ -> {
      let assert Ok(rest) = list.rest(tail)
      list.append(head, rest)
    }
  }
}
