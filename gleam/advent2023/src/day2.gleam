import gleam/dict
import gleam/function.{identity}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import simplifile
import utils/list_utils.{init, lookup}

type Cube =
  #(String, Int)

type Group =
  List(Cube)

type Game =
  List(Group)

fn parse_cube(s: String) -> Cube {
  let assert Ok(#(num, color)) = string.split_once(s, " ")
  let assert Ok(num_) = int.parse(num)
  #(color, num_)
}

// parseGroup :: String -> Group
// parseGroup = map parseCube . splitOn ", "
fn parse_group(s: String) -> Group {
  s |> string.split(", ") |> list.map(parse_cube)
}

// parseGame :: String -> (Int, Game)
// parseGame s = (gameNum, groups)
//   where
//     [gameName, gameGroups] = splitOn ": " s
//     gameNum = read $ drop 5 gameName
//     groups = map parseGroup $ splitOn "; " gameGroups
fn parse_game(s: String) -> #(Int, Game) {
  let assert Ok(#(name, groups)) = string.split_once(s, ": ")
  let assert Ok(num) = int.parse(string.drop_left(name, 5))
  let game = groups |> string.split("; ") |> list.map(parse_group)
  #(num, game)
  // #(1, [[#("", 1)]])
}

// part 1

fn is_group_possible(to_check_against: Group, group: Group) -> Bool {
  group
  |> list.all(fn(t) {
    let #(color, num) = t
    to_check_against
    |> lookup(color)
    |> result.map(fn(b) { num <= b })
    |> result.unwrap(False)
  })
}

fn is_game_possible(to_check_against: Group, game: Game) -> Bool {
  game |> list.all(is_group_possible(to_check_against, _))
}

// part 2

fn calc_game_power(game: Game) -> Int {
  game
  |> list.flat_map(identity)
  |> list.fold(dict.new(), fn(acc, t) {
    let #(color, num) = t
    dict.update(acc, color, fn(option) {
      case option {
        Some(v) -> int.max(v, num)
        None -> num
      }
    })
  })
  |> dict.values
  |> int.product
}

pub fn main() {
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2023/Day2/input.txt")
    |> result.map(string.split(_, "\n"))

  let assert Ok(lines) = init(contents)
  let games = lines |> list.map(parse_game)

  let to_check_against = [#("red", 12), #("green", 13), #("blue", 14)]

  let a =
    games
    |> list.map(fn(game) {
      #(game.0, is_game_possible(to_check_against, game.1))
    })
    |> list.filter(fn(t) { t.1 })
    |> list.map(fn(t) { t.0 })
    |> int.sum()

  io.debug(a)

  let b =
    games
    |> list.map(fn(t) { t.1 })
    |> list.map(calc_game_power)
    |> int.sum()

  io.debug(b)
}
