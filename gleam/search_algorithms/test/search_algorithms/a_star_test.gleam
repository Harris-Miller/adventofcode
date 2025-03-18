import common/grid.{type Point}
import common/int as intc
import common/result as resultc
import gleam/dict
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import search_algorithms/a_star
import simplifile

fn setup_test_state() {
  let alphabet = "abcdefghijklmnopqrstuvwxyz" |> string.split("")
  let height_map =
    list.zip(alphabet, list.range(1, list.length(alphabet)))
    |> dict.from_list()
    |> dict.insert("E", 26)
    |> dict.insert("S", 1)

  let assert Ok(hills) =
    simplifile.read(from: "test/search_algorithms/hills.txt")
    |> result.map(string.trim)
    |> result.map(grid.parse(_, function.identity))

  let manhattan = fn(p1: Point) {
    fn(p2: Point) {
      int.to_float(
        int.absolute_value(p1.0 - p2.0) + int.absolute_value(p1.1 - p2.1),
      )
    }
  }

  let can_move_to = fn(p1: Point, p2: Point) -> Bool {
    let assert Ok(h1) =
      dict.get(hills, p1)
      |> result.then(dict.get(height_map, _))
    let assert Ok(h2) =
      dict.get(hills, p2)
      |> result.then(dict.get(height_map, _))
    h2 - h1 < 2
  }

  // const next = (p: Point): Point[] =>
  // getNeighbors4(p)
  //   .filter(key => grid.has(key))
  //   .filter(n => canMoveTo(p, n));
  let next = fn(p: Point) {
    grid.get_neighbors4(p)
    |> list.filter(dict.has_key(hills, _))
    |> list.filter(can_move_to(p, _))
  }

  #(hills, manhattan, next)
}

pub fn main() {
  gleeunit.main()
}

pub fn a_star_test() {
  let #(hills, manhattan, next) = setup_test_state()
  let es = dict.to_list(hills)
  let assert Ok(#(start, _)) = list.find(es, fn(t) { t.1 == "S" })
  let assert Ok(#(end, _)) = list.find(es, fn(t) { t.1 == "E" })

  let assert Ok(r) =
    a_star.a_star(
      next,
      fn(_, _) { 1.0 },
      manhattan(end),
      fn(state) { state == end },
      start,
    )

  r.0 |> should.equal(42.0)
}
