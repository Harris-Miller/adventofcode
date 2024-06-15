import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

fn determine_area_needed(list: List(Int)) -> Int {
  case list {
    [l, w, h] -> {
      let sides = [l * w, l * w, l * h, l * h, w * h, w * h]
      let assert Ok(min) = list.reduce(sides, int.min)
      int.sum(sides) + min
    }
    _ -> {
      panic as "fuck"
    }
  }
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2015/Day2/input.txt")
    |> result.map(string.split(_, "\n"))

  let assert Ok(rows) =
    contents
    |> list.reverse
    |> list.rest
    |> result.map(list.reverse)
    |> result.map(list.map(_, fn(s) {
      s
      |> string.split("x")
      |> list.map(int.parse)
      |> list.map(result.unwrap(_, 0))
    }))

  let part1 = rows |> list.map(determine_area_needed) |> int.sum
  io.debug(part1)
}
// 
