import common/direction.{type Direction, Up}
import common/grid.{type Grid, type Point}
import common/result as resultc
import common/tuple
import gleam/dict
import gleam/function
import gleam/io
import gleam/list
import gleam/pair
import gleam/result
import gleam/set
import gleam/string
import gleam/yielder.{type Yielder}
import simplifile

fn walk_path(
  grid: Grid(String),
  start_point: Point,
  start_dir: Direction,
) -> Yielder(#(Point, Direction)) {
  yielder.unfold(
    #(start_point, start_dir, dict.get(grid, start_point)),
    fn(state) {
      let #(point, dir, space_result) = state

      use space <- resultc.unwrap_guard(space_result, yielder.Done)
      let next_point = direction.move(point, dir)
      let next_space = dict.get(grid, next_point)

      case next_space {
        Ok("#") ->
          yielder.Next(#(point, dir), #(
            point,
            direction.turn_right(dir),
            Ok(space),
          ))
        _ -> yielder.Next(#(point, dir), #(next_point, dir, next_space))
      }
    },
  )
}

pub fn main() {
  let assert Ok(content) =
    simplifile.read(from: "../../inputs/2024/Day6/input.txt")
    |> result.map(string.trim_end)

  // io.debug(content)

  let grid = grid.parse(content, function.identity)
  let start_point =
    grid
    |> dict.to_list
    |> list.find(fn(t) { t.1 == "^" })
    |> resultc.unwrap_assert
    |> pair.first
  let start_direction = Up

  let points =
    walk_path(grid, start_point, start_direction)
    |> yielder.map(pair.first)
    |> yielder.to_list
    |> set.from_list
  io.debug(set.size(points))
}
