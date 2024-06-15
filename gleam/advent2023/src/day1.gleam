import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile
import utils/list_utils.{init}

fn find_first_digit_l(l: List(String)) -> String {
  let assert Ok(r) =
    l
    |> list.find(fn(d) {
      case int.parse(d) {
        Ok(_) -> True
        Error(_) -> False
      }
    })
  r
}

fn find_first_digit(s: String) -> String {
  s |> string.to_graphemes() |> find_first_digit_l()
}

fn find_last_digit(s: String) -> String {
  s |> string.to_graphemes() |> list.reverse() |> find_first_digit_l()
}

fn first_char_digit(s: String) -> Bool {
  let r = string.first(s) |> result.try(fn(d) { int.parse(d) })
  case r {
    Ok(_) -> True
    Error(_) -> False
  }
}

fn text_to_digit(s: String) {
  case s {
    "one" -> "1"
    "two" -> "2"
    "three" -> "3"
    "four" -> "4"
    "five" -> "5"
    "six" -> "6"
    "seven" -> "7"
    "eight" -> "8"
    "nine" -> "9"
    _ -> panic as "Found non single digit word"
  }
}

const digits_as_text = [
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
]

fn do_collect_digits(s: String, acc: List(String)) -> List(String) {
  case s {
    "" -> acc
    s -> {
      case first_char_digit(s) {
        True -> {
          let assert Ok(d) = string.first(s)
          do_collect_digits(string.drop_left(s, 1), list.append(acc, [d]))
        }
        False -> {
          let r = list.find(digits_as_text, string.starts_with(s, _))
          case r {
            Ok(as_str) -> {
              let d = text_to_digit(as_str)
              do_collect_digits(string.drop_left(s, 1), list.append(acc, [d]))
            }
            Error(_) -> {
              do_collect_digits(string.drop_left(s, 1), acc)
            }
          }
        }
      }
    }
  }
}

fn collect_digits(s: String) -> List(String) {
  do_collect_digits(s, [])
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2023/Day1/input.txt")
    |> result.map(string.split(_, "\n"))

  let assert Ok(lines) = init(contents)

  // io.debug(lines)

  // part 1
  let a =
    lines
    |> list.map(fn(x) {
      let assert Ok(d) = int.parse(find_first_digit(x) <> find_last_digit(x))
      d
    })
    |> int.sum()

  io.debug(a)

  // part 2
  let b =
    lines
    |> list.map(collect_digits)
    |> list.map(fn(x) {
      let assert Ok(f) = list.first(x)
      let assert Ok(l) = list.last(x)
      let assert Ok(d) = int.parse(f <> l)
      d
    })
    |> int.sum()

  io.debug(b)
}
