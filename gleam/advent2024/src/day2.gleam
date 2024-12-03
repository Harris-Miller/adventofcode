import common/function as function_utils
import common/list as list_utils
import common/result as result_utils
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

fn all_are_increasing(list: List(Int)) -> Bool {
  list
  |> list_utils.pairs()
  |> list.all(function_utils.uncurry2(fn(l, r) { l + 1 <= r && l + 3 >= r }))
}

fn all_are_decreasing(list: List(Int)) -> Bool {
  list
  |> list_utils.pairs()
  |> list.all(function_utils.uncurry2(fn(l, r) { l - 1 >= r && l - 3 <= r }))
}

// const tryAgainRemovingOneAtATime = (ref: number[]) =>
//   R.range(0, ref.length).reduce((acc, idx) => {
//     if (acc) return acc;
//     const adjusted = R.remove(idx, 1, ref);
//     return areAllIncreasing(adjusted) || areAllDecreasing(adjusted);
//   }, false);

fn try_again_removing_one_at_a_time(list: List(Int)) -> Bool {
  list.range(0, list.length(list))
  |> list.find(fn(i) {
    let adjusted = list_utils.remove(list, i)
    all_are_increasing(adjusted) || all_are_decreasing(adjusted)
  })
  |> result.replace(True)
  |> result.unwrap(False)
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2024/Day2/input.txt")
    |> result.map(string.trim_end)
    |> result.map(fn(text) {
      text
      |> string.split("\n")
      |> list.map(fn(line) {
        line
        |> string.split(" ")
        |> list.map(int.parse)
        |> list.map(result_utils.unwrap_assert)
      })
    })

  // io.debug(contents)

  let r1 =
    contents
    |> list.map(fn(x) { all_are_increasing(x) || all_are_decreasing(x) })
    |> list.count(function.identity)

  io.debug(r1)

  let r2 =
    contents
    |> list.map(fn(x) {
      all_are_increasing(x)
      || all_are_decreasing(x)
      || try_again_removing_one_at_a_time(x)
    })
    |> list.count(function.identity)

  io.debug(r2)
}
