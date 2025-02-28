import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleam/yielder
import simplifile

fn parse(char: String) -> Int {
  case char {
    "(" -> 1
    ")" -> -1
    _ -> 0
  }
}

fn find_position(xs: List(#(Int, Int))) -> Int {
  case xs {
    [#(v, i), ..rest] ->
      case v {
        -1 -> i
        _ -> find_position(rest)
      }
    [] -> -1
  }
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2015/Day1/input.txt")
    |> result.map(string.split(_, ""))

  // part 1
  let r1 =
    contents
    |> list.map(parse)
    |> int.sum

  io.debug(r1)

  // part 2
  let r2 =
    contents
    |> list.map(parse)
    |> list.scan(0, int.add)
    |> yielder.from_list
    |> yielder.index
    |> yielder.to_list
    |> find_position

  io.debug(r2 + 1)
}
