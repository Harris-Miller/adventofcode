import gleam/function
import gleam/io
import gleam/list
import gleam/set
import gleam/yielder.{type Yielder, Done, Next, find, unfold}

pub fn depth_first_yielder(
  next: fn(a) -> List(a),
  initial: a,
) -> Yielder(List(a)) {
  unfold(from: #([[initial]], set.new()), with: fn(state) {
    let #(stack, visited) = state

    case stack {
      [] -> Done
      [path, ..next_stack] -> {
        let assert Ok(current) = list.first(path)

        case set.contains(visited, current) {
          True -> Next(Error(Nil), #(next_stack, visited))
          False -> {
            let next_visited = set.insert(visited, current)

            let next_states =
              current
              |> next()
              |> list.filter(fn(v) { !set.contains(next_visited, v) })
              |> list.map(fn(v) { [v, ..path] })

            let next_queue =
              list.fold_right(next_states, next_stack, fn(st, v) { [v, ..st] })

            Next(Ok(path), #(next_queue, next_visited))
          }
        }
      }
    }
  })
  |> yielder.filter_map(function.identity)
}

pub fn depth_first_search(
  next: fn(a) -> List(a),
  found: fn(a) -> Bool,
  initial: a,
) -> Result(List(a), Nil) {
  let iterator = depth_first_yielder(next, initial)
  find(iterator, fn(path) {
    let assert Ok(value) = list.first(path)
    found(value)
  })
}
