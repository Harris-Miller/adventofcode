import gleam/dict.{type Dict}
import gleam/function.{identity}
import gleam/io
import gleam/iterator.{iterate, take}
import gleam/list
import gleam/string
import simplifile
import utils/grid

fn next_space(grid: Dict(#(Int, Int), Bool)) {
  fn(key: #(Int, Int), value: Bool) {
    let ns = grid.get_neighbors8(key)

    let n_on =
      ns
      |> list.filter_map(dict.get(grid, _))
      |> list.filter(identity)
      |> list.length()

    case value {
      True if n_on > 1 && n_on < 4 -> True
      True -> False
      False if n_on == 3 -> True
      False -> False
    }
  }
}

fn next_space_corner_always_on(grid: Dict(#(Int, Int), Bool)) {
  let #(row_max, col_max) = grid.get_maxes(grid)
  fn(key: #(Int, Int), value: Bool) {
    case key {
      #(0, 0) -> True
      #(row, 0) if row == row_max -> True
      #(0, col) if col == col_max -> True
      #(row, col) if row == row_max && col == col_max -> True
      _ -> next_space(grid)(key, value)
    }
  }
}

fn step(grid: Dict(#(Int, Int), Bool)) {
  dict.map_values(grid, next_space(grid))
}

fn step2(grid: Dict(#(Int, Int), Bool)) {
  dict.map_values(grid, next_space_corner_always_on(grid))
}

fn turn_corners_on(grid: Dict(#(Int, Int), Bool)) {
  let #(row_max, col_max) = grid.get_maxes(grid)
  grid
  |> dict.insert(#(0, 0), True)
  |> dict.insert(#(row_max, 0), True)
  |> dict.insert(#(0, col_max), True)
  |> dict.insert(#(row_max, col_max), True)
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2015/Day18/input.txt")

  let grid =
    grid.parse(contents, fn(v) {
      case v {
        "#" -> True
        _ -> False
      }
    })

  let assert Ok(r) = iterate(grid, step) |> take(101) |> iterator.last()
  let r = r |> dict.values() |> list.filter(identity) |> list.length()
  io.debug(r)

  let grid2 = turn_corners_on(grid)
  let assert Ok(r) = iterate(grid2, step2) |> take(101) |> iterator.last()
  let r = r |> dict.values() |> list.filter(identity) |> list.length()
  io.debug(r)
  // grid
  //   |> grid.to_string(fn(b) {
  //     case b {
  //       True -> "#"
  //       False -> "."
  //     }
  //   })
  //   |> string.split("\n")
  //   |> list.each(io.debug)

  //   io.debug("")
}
