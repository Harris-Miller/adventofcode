import common/list as list_utils
import common/tuple
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/order.{Eq, Gt, Lt}
import gleam/result
import gleam/string
import simplifile

fn dict_of_things() {
  [
    #("children", 3),
    #("cats", 7),
    #("samoyeds", 2),
    #("pomeranians", 3),
    #("akitas", 0),
    #("vizslas", 0),
    #("goldfish", 5),
    #("trees", 3),
    #("cars", 2),
    #("perfumes", 1),
  ]
  |> dict.from_list()
}

fn part2_opts() {
  [#("cats", Gt), #("trees", Gt), #("pomeranians", Lt), #("goldfish", Lt)]
  |> dict.from_list
}

fn part1_fns() {
  dict_of_things()
  |> dict.map_values(fn(_, v) { fn(other) { other == v } })
}

fn part2_fns() {
  let ops = part2_opts()
  dict_of_things()
  |> dict.map_values(fn(k, v) {
    let op = dict.get(ops, k) |> result.unwrap(Eq)
    case op {
      Gt -> fn(other) { other > v }
      Lt -> fn(other) { other < v }
      Eq -> fn(other) { other == v }
    }
  })
}

fn parse(line: String) {
  let assert Ok(#(_, items)) = line |> string.split_once(": ")
  items
  |> string.split(", ")
  |> list.map(fn(s) {
    let #(thing, num_str) = s |> string.split(": ") |> tuple.from_list2
    let assert Ok(num) = num_str |> int.parse()
    #(thing, num)
  })
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2015/Day16/input.txt")
    |> result.nil_error()
    |> result.then(fn(file) {
      file
      |> string.split("\n")
      |> list_utils.init
      |> result.map(list.map(_, parse))
      |> result.map(list.index_map(_, fn(x, i) { #(i, x) }))
    })

  let part1_fns = part1_fns()

  let assert Ok(#(aunt, _)) =
    contents
    |> list.find(fn(tuple) {
      let #(_, items) = tuple
      items
      |> list.all(fn(item) {
        let #(name, num) = item
        let assert Ok(f) = dict.get(part1_fns, name)
        f(num)
      })
    })
  io.debug(aunt + 1)

  let part2_fns = part2_fns()

  let assert Ok(#(aunt, _)) =
    contents
    |> list.find(fn(tuple) {
      let #(_, items) = tuple
      items
      |> list.all(fn(item) {
        let #(name, num) = item
        let assert Ok(f) = dict.get(part2_fns, name)
        f(num)
      })
    })
  io.debug(aunt + 1)
}
