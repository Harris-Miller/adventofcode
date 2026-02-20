import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/order
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import gleam/string
import simplifile

pub type Part {
  Part1
  Part2
}

pub type Point3 {
  Point3(x: Int, y: Int, z: Int)
}

fn straight_line_distance(points: #(Point3, Point3)) -> Float {
  let #(p1, p2) = points
  let assert Ok(r) =
    [
      int.power(p1.x - p2.x, 2.0),
      int.power(p1.y - p2.y, 2.0),
      int.power(p1.z - p2.z, 2.0),
    ]
    |> result.values()
    |> float.sum()
    |> float.square_root()
  r
}

fn connection_fold(total: Int, part: Part) {
  fn(circuits: Set(Set(Point3)), point_pair: #(Point3, Point3)) {
    // point_pairs is already sorted by shortest distant
    let #(p1, p2) = point_pair

    // create a new dict key'd by a point, value is #(k, v) from circuits in which circuit it currently is in, if any
    let filtered =
      set.fold(circuits, dict.new(), fn(acc, v) {
        list.fold([p1, p2], acc, fn(acc, p) {
          case set.contains(v, p) {
            True -> dict.insert(acc, p, v)
            False -> acc
          }
        })
      })

    // these will be Ok() if the point is in a circuit, Error() if not
    let p1_set_result = dict.get(filtered, p1)
    let p2_set_result = dict.get(filtered, p2)

    let updated_circuits = case p1_set_result, p2_set_result {
      // if both points are already in a circuit
      Ok(p1_set), Ok(p2_set) -> {
        // if they are already in the same set (via previous connections)
        case p1_set == p2_set {
          // return circuits unchanged
          True -> circuits
          False -> {
            // else, remove both keys from circuits
            circuits
            |> set.drop([p1_set, p2_set])
            |> set.insert(set.union(p1_set, p2_set))
          }
        }
      }
      // if one is, add the other to it
      Ok(p1_set), Error(Nil) -> {
        let updated_set = p1_set |> set.insert(p2)
        circuits |> set.drop([p1_set]) |> set.insert(updated_set)
      }
      Error(Nil), Ok(p2_set) -> {
        let updated_set = p2_set |> set.insert(p1)
        circuits |> set.drop([p2_set]) |> set.insert(updated_set)
      }
      // if neither are, create a new circuit
      _, _ -> {
        let new_set = set.from_list([p1, p2])
        circuits |> set.insert(new_set)
      }
    }

    case part {
      Part1 -> Ok(updated_circuits)
      Part2 -> {
        let is_finished = {
          case set.size(updated_circuits) == 1 {
            False -> False
            True -> {
              let assert [only_circuit] = updated_circuits |> set.to_list()
              set.size(only_circuit) == total
            }
          }
        }

        case is_finished {
          True -> Error(point_pair)
          False -> Ok(updated_circuits)
        }
      }
    }
  }
}

pub fn main() {
  let filename = "input"
  let assert Ok(contents) =
    simplifile.read(from: "../../inputs/2025/Day8/" <> filename <> ".txt")
    |> result.map(string.trim)
    |> result.map(string.split(_, "\n"))

  let points =
    list.map(contents, fn(line) {
      let assert [x, y, z] =
        line
        |> string.split(",")
        |> list.map(int.parse)
        |> result.values()

      Point3(x, y, z)
    })

  let total_points = list.length(points)

  let paired_and_measured =
    points
    |> list.combination_pairs()
    |> list.map(fn(points) {
      let distance = straight_line_distance(points)
      #(points, distance)
    })
    |> list.sort(fn(l, r) { float.compare(l.1, r.1) })
    |> list.map(pair.first)

  let to_take = case filename {
    "sample" -> 10
    _ -> 1000
  }

  let assert Ok(connected) =
    paired_and_measured
    |> list.take(to_take)
    |> list.try_fold(set.new(), connection_fold(total_points, Part1))

  let sorted =
    connected
    |> set.to_list()
    |> list.sort(fn(l, r) {
      order.reverse(int.compare)(set.size(l), set.size(r))
    })

  // listc.debug(sorted |> list.map(set.to_list))

  echo sorted |> list.take(3) |> list.map(set.size) |> int.product()

  let assert Error(things) =
    paired_and_measured
    |> list.try_fold(set.new(), connection_fold(total_points, Part2))

  let #(p1, p2) = things
  echo p1.x * p2.x
}
