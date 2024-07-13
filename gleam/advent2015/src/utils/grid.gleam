import gleam/dict.{type Dict}
import gleam/list
import gleam/string
import gleam/string_builder
import utils/list as list_utils
import utils/tuple

pub fn parse(str: String, with parser: fn(String) -> b) {
  let assert Ok(lines) = str |> string.split("\n") |> list_utils.init
  lines
  |> list.index_map(fn(values, row) {
    values
    |> string.split("")
    |> list.index_map(fn(value, col) { #(#(row, col), parser(value)) })
  })
  |> list.flatten()
  |> dict.from_list
}

pub fn get_maxes(grid: Dict(#(Int, Int), a)) -> #(Int, Int) {
  let row_max =
    grid |> dict.keys() |> list.map(tuple.fst) |> list_utils.maximum()
  let col_max =
    grid |> dict.keys() |> list.map(tuple.snd) |> list_utils.maximum()
  #(row_max, col_max)
}

pub fn get_neighbors4(point: #(Int, Int)) {
  let #(row, col) = point
  [#(row, col - 1), #(row + 1, col), #(row, col + 1), #(row - 1, col)]
}

pub fn get_neighbors8(point: #(Int, Int)) {
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

pub fn to_string(
  from grid: Dict(#(Int, Int), a),
  using fun: fn(a) -> String,
) -> String {
  let #(row_max, col_max) = get_maxes(grid)
  let rows = list.range(0, row_max)
  let cols = list.range(0, col_max)

  list.map(rows, fn(row) {
    list.fold(cols, string_builder.new(), fn(sb, col) {
      let assert Ok(v) = dict.get(grid, #(row, col))
      string_builder.append(sb, fun(v))
    })
    |> string_builder.to_string()
  })
  |> string.join("\n")
}
