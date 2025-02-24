import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import gleam/yielder
import gleeunit
import gleeunit/should
import search_algorithms/breadth_first.{
  breadth_first_search, breadth_first_yielder,
}
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn breadth_first_yielder_test() {
  let next = fn(value: String) { list.map(["1", "2"], fn(v) { value <> v }) }

  let r =
    breadth_first_yielder(next, "1")
    |> yielder.take_while(satisfying: fn(state) {
      let assert Ok(val) = list.first(state)
      val != "1111"
    })
    |> yielder.map(fn(state) {
      let assert Ok(val) = list.first(state)
      val
    })
    |> yielder.to_list

  r |> should.equal(["1", "11", "12", "111", "112", "121", "122"])
}

pub fn breadth_first_search_test() {
  let next = fn(value: String) { list.map(["1", "2"], fn(v) { value <> v }) }
  let found = fn(value: String) { value == "122" }

  let results = breadth_first_search(next, found, "1")

  results
  |> should.equal(Ok(["122", "12", "1"]))
}

pub fn cheese_search_test() {
  let assert Ok(contents) =
    simplifile.read(from: "test/search_algorithms/cheeseSearch.txt")
    |> result.map(string.trim)

  let grid = make_grid(contents)

  let walls =
    grid
    |> dict.filter(fn(_, val) { val == "#" })
    |> dict.keys()
    |> set.from_list()

  let assert [start, end] =
    grid
    |> dict.filter(fn(_, val) { val == "0" || val == "7" })
    |> dict.keys()

  let next = fn(point: #(Int, Int)) {
    point
    |> get_neighbors_4()
    |> list.filter(fn(p) { !set.contains(walls, p) })
  }
  let found = fn(point: #(Int, Int)) { point == end }

  let iterator = breadth_first_yielder(next, start)
  let assert Ok(result) =
    yielder.find(iterator, fn(path) {
      let assert Ok(value) = list.first(path)
      found(value)
    })

  // let assert Ok(result) = breadth_first_search(next, found, start)

  list.length(result) |> should.equal(247)
}

fn make_grid(s: String) -> Dict(#(Int, Int), String) {
  let l = string.split(s, "\n")

  let rows =
    list.map(l, fn(row) {
      let cs = yielder.range(0, string.length(row) - 1) |> yielder.to_list
      list.zip(cs, string.split(row, ""))
    })

  yielder.range(0, list.length(rows) - 1)
  |> yielder.to_list
  |> list.zip(rows)
  |> list.flat_map(fn(arg) {
    let #(r, cols) = arg
    list.map(cols, fn(col) {
      let #(c, v) = col
      #(#(r, c), v)
    })
  })
  |> dict.from_list
}

fn get_neighbors_4(point: #(Int, Int)) {
  let #(r, c) = point
  [#(r - 1, c), #(r, c + 1), #(r + 1, c), #(r, c - 1)]
}
