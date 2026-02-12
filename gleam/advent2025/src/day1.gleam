import gleam/int
import gleam/list
import gleam/result
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2025/Day1/input.txt")
    |> result.map(string.trim_end)
    |> result.map(string.split(_, "\n"))

  let r =
    list.map(contents, fn(val) {
      let assert Ok(#(head, tail)) = string.pop_grapheme(val)
      let assert Ok(num) = int.parse(tail)
      #(head, num)
    })
    |> list.fold(#(0, 50), fn(acc, x) {
      let #(zeros, dial) = acc
      let #(dir, rotate) = x
      let next_dial = case dir {
        "L" -> dial - rotate
        _ -> dial + rotate
      }

      let #(zeros, next_dial) = case next_dial {
        100 | -100 -> #(zeros + 1, 0)
        _ -> #(zeros, next_dial)
      }

      let zeros = zeros + int.absolute_value(next_dial / 100)
      let assert Ok(next_dial) = int.modulo(next_dial, 100)

      let zeros = case next_dial, dial {
        _, _
          if next_dial == 0
          || next_dial < 0
          && dial > 0
          || next_dial > 0
          && dial < 0
        -> zeros + 1
        _, _ -> zeros
      }

      let next_dial = case next_dial {
        _ if next_dial < 0 -> next_dial + 100
        _ -> next_dial
      }

      #(zeros, next_dial)
    })

  echo r
}
