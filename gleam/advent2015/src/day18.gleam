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

fn step(grid: Dict(#(Int, Int), Bool)) {
  dict.map_values(grid, next_space(grid))
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

  let assert Ok(grid) = iterate(grid, step) |> take(101) |> iterator.last()
  let r = grid |> dict.values() |> list.filter(identity) |> list.length()
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
