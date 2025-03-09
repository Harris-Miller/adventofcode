import common/list as listc
import common/result as resultc
import common/tuple as tuplec
import gleam/int
import gleam/io
import gleam/list
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import simplifile

fn in_right_order(rule: #(String, String), page: List(String)) {
  let #(r1, r2) = rule
  let pages_i = listc.with_index(page)
  let i1_result =
    list.find(pages_i, fn(t) { t.0 == r1 }) |> result.map(tuplec.snd)
  let i2_result =
    list.find(pages_i, fn(t) { t.0 == r2 }) |> result.map(tuplec.snd)

  // if neither are not found, that means the rule doesn't apply to these pages, in which case just return True
  use i1 <- resultc.unwrap_guard(i1_result, True)
  use i2 <- resultc.unwrap_guard(i2_result, True)
  i1 < i2
}

// const getMiddleValue = <T>(arr: T[]): T => {
//   const i = Math.floor(arr.length / 2);
//   return arr[i];
// };
fn get_middle_value(l: List(a)) -> a {
  l
  |> list.split(list.length(l) / 2)
  |> tuplec.snd
  |> list.first
  |> resultc.unwrap_assert
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2024/Day5/input.txt")
    |> result.map(string.trim_end)

  let #(rules, pages) = {
    let assert [a, b] = string.split(contents, "\n\n")
    let aa =
      string.split(a, "\n")
      |> list.map(string.split(_, "|"))
      |> list.map(tuplec.from_list2)

    let bb =
      string.split(b, "\n")
      |> list.map(string.split(_, ","))

    #(aa, bb)
  }

  let #(correct_order, incorrect_order) =
    list.partition(pages, fn(page) {
      list.all(rules, fn(rule) { in_right_order(rule, page) })
    })

  let r1 =
    correct_order
    |> list.map(get_middle_value)
    |> list.map(int.parse)
    |> list.map(resultc.unwrap_assert)
    |> int.sum
  io.debug(r1)

  // const updated = incorrectOrder.map(line =>
  //   R.sort((l, r) => (rules.find(([ll, rr]) => r === rr && l === ll) ? -1 : 1), line),
  // );
  let updated =
    incorrect_order
    |> list.map(fn(line) {
      line
      |> list.sort(fn(l, r) {
        let found =
          list.find(rules, fn(rule) {
            let #(ll, rr) = rule
            r == rr && l == ll
          })
        case found {
          Ok(_) -> Gt
          Error(_) -> Lt
        }
      })
    })

  let r2 =
    updated
    |> list.map(get_middle_value)
    |> list.map(int.parse)
    |> list.map(resultc.unwrap_assert)
    |> int.sum
  io.debug(r2)
}
