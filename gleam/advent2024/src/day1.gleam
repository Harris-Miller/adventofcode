import common/dict as dictc
import common/function as functionc
import common/pair as pairc
import common/result as resultc
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2024/Day1/input.txt")
    |> result.map(string.trim_end)
    |> result.map(string.split(_, "\n"))

  let pairs =
    contents
    |> list.map(fn(line) {
      line
      |> string.split("   ")
      |> list.map(int.parse)
      |> list.map(resultc.unwrap_assert)
      |> pairc.from_list2
    })

  // io.debug(pairs)

  let #(ll, rr) = list.unzip(pairs)
  let sorted = list.zip(list.sort(ll, int.compare), list.sort(rr, int.compare))
  let r1 =
    sorted
    |> list.map(functionc.uncurry2(int.subtract))
    |> list.map(int.absolute_value)
    |> int.sum

  io.debug(r1)

  let counts =
    rr
    |> list.fold(dict.new(), fn(acc, key) {
      dictc.insert_with(acc, key, 1, int.add)
    })

  let r2 =
    ll
    |> list.map(fn(key) {
      dict.get(counts, key) |> result.map(int.multiply(_, key))
    })
    |> result.partition
    |> pair.first
    |> int.sum

  io.debug(r2)
}
