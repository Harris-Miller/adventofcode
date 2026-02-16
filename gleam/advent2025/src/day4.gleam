import common/grid.{type Grid}
import gleam/dict
import gleam/function
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import simplifile

fn filter_accessible(grid: Grid(String)) {
  grid
  |> dict.to_list()
  |> list.filter(fn(kvp) { kvp.1 == "@" })
  |> list.filter(fn(kvp) {
    let ns = grid.get_neighbors8(kvp.0)
    let #(found, _) =
      list.map(ns, fn(p) { dict.get(grid, p) |> result.map(pair.new(p, _)) })
      |> result.partition()

    found |> list.filter(fn(kvp2) { kvp2.1 == "@" })
    let count =
      found |> list.filter(fn(kvp2) { kvp2.1 == "@" }) |> list.length()

    count < 4
  })
}

fn remove_until_done(grid: Grid(String)) {
  let to_remove = filter_accessible(grid) |> list.map(pair.first)

  case list.length(to_remove) {
    l if l == 0 -> grid
    _ -> {
      let updated_grid = list.fold(to_remove, grid, dict.delete)
      remove_until_done(updated_grid)
    }
  }
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2025/Day4/input.txt")
    |> result.map(string.trim_end)

  let grid = grid.parse(contents, function.identity)

  let can_access = filter_accessible(grid)

  echo list.length(can_access)

  let as_much_removed = remove_until_done(grid)

  let starting_total =
    grid |> dict.values() |> list.filter(fn(v) { v == "@" }) |> list.length()
  let ending_total =
    as_much_removed
    |> dict.values()
    |> list.filter(fn(v) { v == "@" })
    |> list.length()

  echo starting_total - ending_total
}
