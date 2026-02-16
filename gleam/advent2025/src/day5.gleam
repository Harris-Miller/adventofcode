import common/result as resultc
import gleam/int
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import simplifile

fn parse(contents: String) {
  let assert [ranges, ids] = string.split(contents, "\n\n")

  let ranges =
    ranges
    |> string.split("\n")
    |> list.map(fn(range) {
      let assert [from, to] = string.split(range, "-")
      let assert Ok(from) = int.parse(from)
      let assert Ok(to) = int.parse(to)
      #(from, to)
    })

  let ids =
    ids
    |> string.split("\n")
    |> list.map(fn(s) {
      let assert Ok(v) = int.parse(s)
      v
    })

  #(ranges, ids)
}

fn in_range(range: #(Int, Int), id: Int) {
  let #(from, to) = range
  from <= id && id <= to
}

fn is_fresh(ranges: List(#(Int, Int)), id: Int) {
  list.any(ranges, in_range(_, id))
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2025/Day5/input.txt")
    |> result.map(string.trim_end)

  let #(ranges, ids) = parse(contents)

  let fresh = ids |> list.filter(is_fresh(ranges, _))
  echo list.length(fresh)
}
