import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(groups) =
    simplifile.read(from: "../../inputs/2020/Day6/input.txt")
    |> result.map(string.trim)
    |> result.map(string.split(_, "\n\n"))
    |> result.map(list.map(_, string.split(_, "\n")))

  let part1 =
    groups
    |> list.map(fn(g) {
      g |> string.concat() |> string.split("") |> set.from_list() |> set.size()
    })
    |> int.sum()
    |> int.to_string()

  io.println(part1)

  let part2 =
    groups
    |> list.map(fn(g) {
      g
      |> list.map(fn(x) { x |> string.split("") |> set.from_list() })
      |> list.reduce(set.intersection)
      |> result.unwrap(set.new())
      |> set.size()
    })
    |> int.sum()
    |> int.to_string()

  io.println(part2)
}
