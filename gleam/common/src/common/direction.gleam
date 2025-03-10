import common/grid.{type Point}

pub type Direction {
  Up
  Right
  Down
  Left
}

pub fn move(from point: Point, moving direction: Direction) -> Point {
  let #(r, c) = point
  case direction {
    Up -> #(r - 1, c)
    Right -> #(r, c + 1)
    Down -> #(r + 1, c)
    Left -> #(r, c - 1)
  }
}

pub fn turn_right(from direction: Direction) -> Direction {
  case direction {
    Up -> Right
    Right -> Down
    Down -> Left
    Left -> Up
  }
}
