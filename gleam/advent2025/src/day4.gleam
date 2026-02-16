import common/grid
import gleam/dict
import gleam/function
import gleam/int
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2025/Day4/input.txt")
    |> result.map(string.trim_end)

  let grid = grid.parse(contents, function.identity)

  let can_access =
    grid
    |> dict.to_list()
    |> list.filter(fn(kvp) { kvp.1 == "@" })
    |> list.filter(fn(kvp) {
      let ns = grid.get_neighbors8(kvp.0)
      let #(found, _) =
        list.map(ns, fn(p) { dict.get(grid, p) |> result.map(pair.new(p, _)) })
        |> result.partition()

      let count =
        found |> list.filter(fn(kvp2) { kvp2.1 == "@" }) |> list.length()

      count < 4
    })

  echo list.length(can_access)
}
