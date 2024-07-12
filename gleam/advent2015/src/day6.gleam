import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/string
import simplifile
import utils/listutils

pub type Inst {
  On
  Off
  Toggle
}

fn to_tuple(s: String) -> #(Int, Int) {
  let assert [x, y] =
    s
    |> string.split(",")
    |> list.map(fn(s) {
      let assert Ok(n) = int.parse(s)
      n
    })
  #(x, y)
}

fn parse(line: String) {
  let as_list = line |> string.split(" ")

  let #(inst, rest) = case as_list {
    ["turn", "on", ..] -> #(On, list.drop(as_list, 2))
    ["turn", "off", ..] -> #(Off, list.drop(as_list, 2))
    _ -> #(Toggle, list.drop(as_list, 1))
  }

  let assert Ok(start) = list.first(rest) |> result.map(to_tuple)
  let assert Ok(end) = list.last(rest) |> result.map(to_tuple)

  #(inst, start, end)
}

fn ranges_to_points(from: #(Int, Int), to: #(Int, Int)) {
  let #(from_row, from_col) = from
  let #(to_row, to_col) = to
  let cols = list.range(from_col, to_col)
  let rows = list.range(from_row, to_row)
  list.flat_map(rows, fn(row) { list.map(cols, fn(col) { #(col, row) }) })
}

fn make_grid() {
  ranges_to_points(#(0, 0), #(999, 999))
  |> list.map(fn(a) { #(a, False) })
  |> dict.from_list
}

fn process(
  grid: Dict(#(Int, Int), Bool),
  line: #(Inst, #(Int, Int), #(Int, Int)),
) {
  let #(inst, from, to) = line
  let points = ranges_to_points(from, to)
  case inst {
    On ->
      dict.merge(
        grid,
        points |> list.map(fn(p) { #(p, True) }) |> dict.from_list,
      )
    Off ->
      dict.merge(
        grid,
        points |> list.map(fn(p) { #(p, False) }) |> dict.from_list,
      )
    Toggle ->
      list.fold(points, grid, fn(acc, p) {
        dict.upsert(acc, p, fn(v) {
          let assert Some(v) = v
          bool.negate(v)
        })
      })
  }
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2015/Day6/input.txt")
    |> result.nil_error()
    |> result.then(fn(file) {
      string.split(file, "\n")
      |> listutils.init
      |> result.map(list.map(_, parse))
    })

  let grid = make_grid()

  let r =
    contents
    |> list.fold(grid, process)
    |> dict.filter(fn(_, v) { v })
    |> dict.size
  io.debug(r)
}
