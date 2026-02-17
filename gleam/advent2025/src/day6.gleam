import gleam/bool
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import simplifile

fn part1(contents: List(String)) {
  let lines =
    contents
    |> list.map(fn(entry) {
      entry
      |> string.trim()
      |> string.split(" ")
      |> list.filter(fn(v) { v != "" })
    })
  let assert [last, ..init] = lines |> list.reverse()
  let sequences =
    init |> list.reverse() |> list.transpose() |> list.zip(last, _)

  let answer =
    sequences
    |> list.map(fn(t) {
      let parsed = t.1 |> list.map(int.parse) |> result.values()
      case t.0 {
        "+" -> int.sum(parsed)
        _ -> int.product(parsed)
      }
    })
    |> int.sum()

  echo answer
}

fn split_on_empties(lists: List(List(String))) {
  use <- bool.guard(list.is_empty(lists), [])

  let #(head, rest) =
    list.split_while(lists, fn(list) { !list.all(list, fn(v) { v == " " }) })

  let rest = rest |> list.drop(1) |> split_on_empties()
  [head, ..rest]
}

fn split_ops(lists: List(List(String))) {
  let assert [op_str, ..rest] =
    list.transpose(lists)
    |> list.reverse()

  let op = op_str |> string.join("") |> string.trim()

  let rest =
    rest
    |> list.reverse()
    |> list.transpose()

  let rest =
    rest
    |> list.map(fn(val) {
      val |> string.join("") |> string.trim() |> int.parse()
    })
    |> result.values()

  #(op, rest)
}

fn apply_op(tuple: #(String, List(Int))) {
  let #(op, vals) = tuple

  case op {
    "*" -> int.product(vals)
    _ -> int.sum(vals)
  }
}

fn part2(contents: List(String)) {
  let trans = contents |> list.map(string.split(_, "")) |> list.transpose()

  let answer =
    trans
    |> split_on_empties()
    |> list.map(split_ops)
    |> list.map(apply_op)
    |> int.sum()

  echo answer
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2025/Day6/input.txt")
    |> result.map(fn(str) {
      str |> string.split("\n") |> list.map(string.to_option) |> option.values()
    })

  part1(contents)
  part2(contents)
}
