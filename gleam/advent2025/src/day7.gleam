import gleam/bool
import gleam/list
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import gleam/string
import simplifile

fn do_it(tachyons: Set(Int), lines: List(List(#(Int, String)))) {
  use <- bool.guard(list.is_empty(lines), 0)

  let assert [head, ..rest] = lines

  let #(splits, pass_through) =
    head
    |> list.filter(fn(kvp) { set.contains(tachyons, kvp.0) })
    |> list.partition(fn(kvp) { kvp.1 == "^" })
    |> pair.map_first(fn(list) { list |> list.map(pair.first) })
    |> pair.map_second(fn(list) { list |> list.map(pair.first) })

  // echo splits
  // echo pass_through

  let new_indexes = splits |> list.flat_map(fn(x) { [x - 1, x + 1] })

  let new_tachyons = list.append(pass_through, new_indexes) |> set.from_list()

  list.length(splits) + do_it(new_tachyons, rest)
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2025/Day7/input.txt")
    |> result.map(string.split(_, "\n"))

  let indexed =
    contents
    |> list.map(fn(row) {
      row |> string.split("") |> list.index_map(fn(x, i) { #(i, x) })
    })

  let assert [head, ..rest] = indexed

  let assert Ok(starting_index) =
    head |> list.find(fn(kvp) { kvp.1 == "S" }) |> result.map(pair.first)

  let s = set.from_list([starting_index])

  echo do_it(s, rest)
}
