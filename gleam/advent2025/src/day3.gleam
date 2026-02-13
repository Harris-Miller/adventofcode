import gleam/int
import gleam/list
import gleam/result
import gleam/string
import simplifile

// Note: since we're dealing with single digits, we can keep things as strings
fn calc_largest_joltage(list: List(String)) {
  let len = list.length(list)
  let indexed = list.index_map(list, fn(x, i) { #(i, x) })
  // find index of max
  let assert Ok(max) = list.max(indexed, fn(l, r) { string.compare(l.1, r.1) })

  case max.0 == len - 1 {
    True -> {
      let rest = list.take(list, len - 1)
      let assert Ok(other) = list.max(rest, string.compare)
      [other, max.1]
    }
    False -> {
      let rest = list.drop(list, max.0 + 1)
      let assert Ok(other) = list.max(rest, string.compare)
      [max.1, other]
    }
  }
}

// the trick is cut the list so we stop searching so there are enough remaining to make 12
fn calc_largest_joltage2(list: List(String)) {
  let indexed = list.index_map(list, fn(x, i) { #(i, x) })

  int.range(11, -1, [], fn(acc, left) {
    let start = case acc {
      [#(i, _), ..] -> i
      [] -> -1
    }
    let cut = indexed |> list.drop(start + 1)
    let cut = list.take(cut, list.length(cut) - left)
    let assert Ok(max) = list.max(cut, fn(l, r) { string.compare(l.1, r.1) })
    [max, ..acc]
  })
  |> list.reverse()
  |> list.map(fn(t) { t.1 })
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2025/Day3/input.txt")
    |> result.map(string.trim_end)
    |> result.map(string.split(_, "\n"))

  let assert Ok(r) =
    contents
    |> list.map(string.split(_, ""))
    |> list.map(calc_largest_joltage)
    |> list.map(string.join(_, ""))
    |> list.map(int.parse)
    |> result.all()
    |> result.map(int.sum)

  echo r

  let assert Ok(r2) =
    contents
    |> list.map(string.split(_, ""))
    |> list.map(calc_largest_joltage2)
    |> list.map(string.join(_, ""))
    |> list.map(int.parse)
    |> result.all()
    |> result.map(int.sum)

  echo r2
}
