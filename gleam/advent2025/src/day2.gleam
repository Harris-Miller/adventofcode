import gleam/int
import gleam/list
import gleam/result
import gleam/string
import gleam/yielder
import ranger
import simplifile

fn doubles(num: String) {
  let len = string.length(num)
  let half = len / 2

  let assert Ok(#(front, back)) =
    string.split(num, "")
    |> list.sized_chunk(half)
    |> list.map(string.join(_, ""))
    |> list.combination_pairs()
    |> list.first()

  front == back
}

fn is_invalid(num: String) {
  let len = string.length(num)
  case int.is_even(len) {
    True -> doubles(num)
    False -> False
  }
}

fn has_repeats(num: String) {
  let len = string.length(num)
  let half = len / 2
  let to_check = int.range(1, half + 1, [], list.prepend)

  let as_list = string.split(num, "")
  list.any(to_check, fn(chunk_by) {
    as_list
    |> list.sized_chunk(chunk_by)
    |> list.map(string.join(_, ""))
    |> list.combination_pairs()
    |> list.all(fn(tuple) { tuple.0 == tuple.1 })
  })
}

fn is_invalid2(num: String) {
  let len = string.length(num)
  case int.is_even(len) {
    True -> has_repeats(num)
    False -> False
  }
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2025/Day2/input.txt")
    |> result.map(string.trim_end)

  let ranges =
    string.split(contents, ",")
    |> list.map(fn(s) {
      let assert [start, end] = string.split(s, "-")
      let assert Ok(start) = int.parse(start)
      let assert Ok(end) = int.parse(end)
      #(start, end)
    })

  let inclusive_range =
    ranger.create(
      validate: fn(_) { True },
      negate_step: fn(s) { -1 * s },
      add: fn(a, b) { a + b },
      compare: int.compare,
    )

  let expanded =
    ranges
    |> list.map(fn(pair) {
      let #(start, end) = pair
      let assert Ok(nums) = inclusive_range(start, end, 1)
      yielder.to_list(nums) |> list.map(int.to_string)
    })

  let r =
    expanded
    |> list.map(list.filter(_, is_invalid))
    |> list.flatten()
    |> list.map(fn(str) {
      let assert Ok(num) = int.parse(str)
      num
    })
    |> int.sum()

  echo r

  let r2 =
    expanded
    |> list.map(list.filter(_, is_invalid2))
    |> list.flatten()
    |> list.map(fn(str) {
      let assert Ok(num) = int.parse(str)
      num
    })
    |> int.sum()

  echo r2
}
