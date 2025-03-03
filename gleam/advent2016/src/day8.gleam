import common/grid.{type Grid}
import common/list as listc
import gleam/dict
import gleam/function.{identity}
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

pub type Instruction {
  Rect(Int, Int)
  RotateC(Int, Int)
  RotateR(Int, Int)
  Noop
}

fn parse(line: String) {
  let line = line |> string.split(" ")
  case line {
    ["rect", xy] -> {
      let assert [Ok(x), Ok(y)] = xy |> string.split("x") |> list.map(int.parse)
      Rect(x, y)
    }
    ["rotate", "column", col, _, a] -> {
      let assert Ok(col) =
        col
        |> string.split("")
        |> list.drop(2)
        |> string.join("")
        |> int.parse()
      let assert Ok(a) = a |> int.parse()
      RotateC(col, a)
    }
    ["rotate", "row", row, _, a] -> {
      let assert Ok(row) =
        row
        |> string.split("")
        |> list.drop(2)
        |> string.join("")
        |> int.parse()
      let assert Ok(a) = a |> int.parse()
      RotateR(row, a)
    }
    _ -> Noop
  }
}

fn do_rect(grid: Grid(Bool), inst: Instruction) {
  let assert Rect(cols, rows) = inst
  let new_squares =
    grid.square_from_points(#(0, 0), #(rows - 1, cols - 1))
    |> list.map(fn(p) { #(p, True) })
    |> dict.from_list
  dict.merge(grid, new_squares)
}

fn do_rotate_c(max_row: Int, grid: Grid(a), inst: Instruction) {
  let assert RotateC(col, a) = inst
  let cols =
    grid
    |> dict.filter(fn(k, _) {
      let #(_, c) = k
      col == c
    })
    |> dict.to_list()
    |> list.map(fn(tuple) {
      let #(#(r, c), v) = tuple
      let assert Ok(new_r) = int.modulo(r + a, max_row + 1)
      #(#(new_r, c), v)
    })
    |> dict.from_list
  dict.merge(grid, cols)
}

fn do_rotate_r(max_col: Int, grid: Grid(a), inst: Instruction) {
  let assert RotateR(row, a) = inst
  let rows =
    grid
    |> dict.filter(fn(k, _) {
      let #(r, _) = k
      row == r
    })
    |> dict.to_list()
    |> list.map(fn(tuple) {
      let #(#(r, c), v) = tuple
      let assert Ok(new_c) = int.modulo(c + a, max_col + 1)
      #(#(r, new_c), v)
    })
    |> dict.from_list
  dict.merge(grid, rows)
}

fn process_instruction(maxes: #(Int, Int)) {
  let #(row_max, col_max) = maxes
  fn(grid: Grid(Bool), inst: Instruction) {
    case inst {
      Rect(_, _) -> do_rect(grid, inst)
      RotateC(_, _) -> do_rotate_c(row_max, grid, inst)
      RotateR(_, _) -> do_rotate_r(col_max, grid, inst)
      Noop -> grid
    }
  }
}

pub fn main() {
  let assert Ok(instructions) =
    simplifile.read(from: "../../inputs/2016/Day8/input.txt")
    |> result.replace_error(Nil)
    |> result.map(string.split(_, "\n"))
    |> result.then(listc.init)
    |> result.map(list.map(_, parse))

  let grid = grid.make_empty(6, 50, False)
  let maxes = grid.get_maxes(grid)

  let grid = list.fold(instructions, grid, process_instruction(maxes))

  let r = grid |> dict.values() |> list.filter(identity) |> list.length()
  io.debug(r)
  io.println("")
  grid.debug(grid, grid.to_std_char)
}
