import gleam/int
import gleam/list
import gleam/result
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2025/Day6/input.txt")
    |> result.map(string.trim_end)
    |> result.map(string.split(_, "\n"))

  let lines =
    contents
    |> list.map(fn(entry) {
      string.split(entry, " ") |> list.filter(fn(v) { v != "" })
    })

  let assert [last, ..init] = lines |> list.reverse()
  let init = init |> list.reverse()

  let sequences = init |> list.transpose() |> list.zip(last, _)

  let part1 =
    sequences
    |> list.map(fn(t) {
      let parsed = t.1 |> list.map(int.parse) |> result.values()
      case t.0 {
        "+" -> int.sum(parsed)
        _ -> int.product(parsed)
      }
    })
    |> int.sum()

  echo part1
}
