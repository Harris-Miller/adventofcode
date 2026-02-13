import common/list as listc
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/string
import gleam/string_tree

pub type Point =
  #(Int, Int)

pub type Grid(a) =
  Dict(Point, a)

pub fn make_empty(num_rows: Int, num_cols: Int, fill: a) -> Grid(a) {
  let rows = int.range(0, num_rows, [], list.prepend) |> list.reverse()
  let cols = int.range(0, num_cols - 1, [], list.prepend) |> list.reverse()

  list.flat_map(rows, fn(row) {
    list.map(cols, fn(col) { #(#(row, col), fill) })
  })
  |> dict.from_list
}

pub fn parse(str: String, with parser: fn(String) -> b) {
  str
  |> string.trim()
  |> string.split("\n")
  |> list.index_map(fn(values, row) {
    values
    |> string.split("")
    |> list.index_map(fn(value, col) { #(#(row, col), parser(value)) })
  })
  |> list.flatten()
  |> dict.from_list
}

pub fn get_maxes(grid: Grid(a)) -> Point {
  let row_max = grid |> dict.keys() |> list.map(pair.first) |> listc.maximum()
  let col_max = grid |> dict.keys() |> list.map(pair.second) |> listc.maximum()
  #(row_max, col_max)
}

pub fn get_neighbors4(point: Point) {
  let #(row, col) = point
  [#(row, col - 1), #(row + 1, col), #(row, col + 1), #(row - 1, col)]
}

pub fn get_neighbors8(point: Point) {
  let #(row, col) = point
  [
    #(row - 1, col - 1),
    #(row - 1, col),
    #(row - 1, col + 1),
    #(row, col + 1),
    #(row + 1, col + 1),
    #(row + 1, col),
    #(row + 1, col - 1),
    #(row, col - 1),
  ]
}

pub fn to_string(from grid: Grid(a), using fun: fn(a) -> String) -> String {
  let #(row_max, col_max) = get_maxes(grid)
  let rows = int.range(0, row_max, [], list.prepend) |> list.reverse()
  let cols = int.range(0, col_max, [], list.prepend) |> list.reverse()

  list.map(rows, fn(row) {
    list.fold(cols, string_tree.new(), fn(sb, col) {
      let assert Ok(v) = dict.get(grid, #(row, col))
      string_tree.append(sb, fun(v))
    })
    |> string_tree.to_string()
  })
  |> string.join("\n")
}

pub fn debug(grid: Grid(a), using fun: fn(a) -> String) {
  grid
  |> to_string(fun)
  |> string.split("\n")
  |> list.each(io.println)
  io.println("")
}

pub fn square_from_points(btm_left: #(Int, Int), top_right: #(Int, Int)) {
  let #(r1, c1) = btm_left
  let #(r2, c2) = top_right
  let rows = int.range(r1, r2, [], list.prepend) |> list.reverse()
  let cols = int.range(c1, c2, [], list.prepend) |> list.reverse()
  list.flat_map(rows, fn(row) { list.map(cols, fn(col) { #(row, col) }) })
}

pub fn to_std_char(b: Bool) -> String {
  case b {
    False -> "."
    True -> "#"
  }
}
