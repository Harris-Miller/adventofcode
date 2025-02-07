import common/result as resultc
import gleam/int
import gleam/io
import gleam/list
import gleam/regexp
import gleam/result
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2024/Day3/input.txt")
    |> result.map(string.trim_end)
    |> result.map(string.split(_, "\n"))
    |> result.map(string.join(_, ""))

  // io.debug(contents)

  let assert Ok(re1) = regexp.from_string("mul\\(\\d{1,4},\\d{1,4}\\)")
  let r1 =
    regexp.scan(re1, contents)
    |> list.map(fn(m) {
      let regexp.Match(c, _) = m
      c
      |> string.drop_start(4)
      |> string.drop_end(1)
      |> string.split(",")
      |> list.map(fn(a) { int.parse(a) |> resultc.unwrap_assert() })
      |> int.product()
    })
    |> int.sum()

  io.debug(r1)
}
