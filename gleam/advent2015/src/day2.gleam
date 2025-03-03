import common/list as listc
import common/result as resultc
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

fn determine_length_needed(list: List(Int)) -> Int {
  list
  |> list.sort(int.compare)
  |> listc.init
  |> resultc.unwrap_assert
  |> list.flat_map(fn(x) { [x, x] })
  |> int.sum
  |> fn(l) { int.product(list) + l }
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
    |> result.map(
      list.map(_, fn(s) {
        s
        |> string.split("x")
        |> list.map(int.parse)
        |> list.map(result.unwrap(_, 0))
      }),
    )

  let part1 = rows |> list.map(determine_area_needed) |> int.sum
  io.debug(part1)

  let part2 = rows |> list.map(determine_length_needed) |> int.sum
  io.debug(part2)
}
