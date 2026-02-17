import gleam/int
import gleam/list
import gleam/result
import gleam/string
import simplifile

fn parse(contents: String) {
  let assert [ranges, ids] = string.split(contents, "\n\n")

  let ranges =
    ranges
    |> string.split("\n")
    |> list.map(fn(range) {
      let assert [from, to] = string.split(range, "-")
      let assert Ok(from) = int.parse(from)
      let assert Ok(to) = int.parse(to)
      #(from, to)
    })

  let ids =
    ids
    |> string.split("\n")
    |> list.map(fn(s) {
      let assert Ok(v) = int.parse(s)
      v
    })

  #(ranges, ids)
}

fn in_range(range: #(Int, Int), id: Int) {
  let #(from, to) = range
  from <= id && id <= to
}

fn is_fresh(ranges: List(#(Int, Int)), id: Int) {
  list.any(ranges, in_range(_, id))
}

fn adjust_range(
  ranges_stack: List(#(Int, Int)),
  updated_ranges_stack: List(#(Int, Int)),
) -> List(#(Int, Int)) {
  case ranges_stack {
    [] -> panic
    [only] -> updated_ranges_stack |> list.prepend(only) |> list.reverse()
    [first, second, ..ranges] -> {
      case first.1 < second.0 {
        // if the end of the first is less than the second, second goes back on stack, first gets added to updated
        True ->
          adjust_range([second, ..ranges], [first, ..updated_ranges_stack])
        False -> {
          // else, merge the ranges by starting from first and going to which ever has a higher end
          let merged_ranges = #(first.0, int.max(first.1, second.1))
          // then place merged_ranges on the main stack to reprocess
          adjust_range([merged_ranges, ..ranges], updated_ranges_stack)
        }
      }
    }
  }
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2025/Day5/input.txt")
    |> result.map(string.trim_end)

  let #(ranges, ids) = parse(contents)

  let fresh = ids |> list.filter(is_fresh(ranges, _))
  echo list.length(fresh)

  let adjusted_ranged =
    ranges |> list.sort(fn(a, b) { int.compare(a.0, b.0) }) |> adjust_range([])

  let total =
    adjusted_ranged
    |> list.map(fn(t) { t.1 - t.0 + 1 })
    |> int.sum()

  echo total
}
