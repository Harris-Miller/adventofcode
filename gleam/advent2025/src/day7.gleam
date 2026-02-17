import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import gleam/string
import simplifile

fn part1(tachyons: Set(Int), lines: List(List(#(Int, String)))) {
  use <- bool.guard(list.is_empty(lines), 0)

  let assert [head, ..rest] = lines

  let #(splits, pass_through) =
    head
    |> list.filter(fn(kvp) { set.contains(tachyons, kvp.0) })
    |> list.partition(fn(kvp) { kvp.1 == "^" })
    |> pair.map_first(fn(list) { list |> list.map(pair.first) })
    |> pair.map_second(fn(list) { list |> list.map(pair.first) })

  let new_indexes = splits |> list.flat_map(fn(x) { [x - 1, x + 1] })

  let new_tachyons = list.append(pass_through, new_indexes) |> set.from_list()

  list.length(splits) + part1(new_tachyons, rest)
}

fn insert_or_append(opt: Option(Int), new_value: Int) {
  case opt {
    None -> new_value
    Some(value) -> value + new_value
  }
}

fn part2(tachyons: Dict(Int, Int), lines: List(List(#(Int, String)))) {
  use <- bool.guard(list.is_empty(lines), tachyons)

  let assert [head, ..rest] = lines

  let #(split, pass_through) =
    head
    |> list.filter(fn(kvp) { dict.has_key(tachyons, kvp.0) })
    |> list.partition(fn(kvp) { kvp.1 == "^" })
    |> pair.map_first(fn(list) { list |> list.map(pair.first) })
    |> pair.map_second(fn(list) { list |> list.map(pair.first) })

  let new_tachyons = dict.take(tachyons, pass_through)

  let new_tachyons =
    list.fold(split, new_tachyons, fn(acc, i) {
      let assert Ok(x) = dict.get(tachyons, i)
      acc
      |> dict.upsert(i - 1, insert_or_append(_, x))
      |> dict.upsert(i + 1, insert_or_append(_, x))
    })

  part2(new_tachyons, rest)
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2025/Day7/input.txt")
    |> result.map(string.trim)
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
  echo part1(s, rest)

  let d = dict.from_list([#(starting_index, 1)])
  let r2 = part2(d, rest)
  echo r2 |> dict.values() |> int.sum()
}
