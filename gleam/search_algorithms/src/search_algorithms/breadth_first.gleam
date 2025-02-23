import gleam/deque.{pop_front}
import gleam/list
import gleam/option.{None, Some, to_result}
import gleam/result.{try}
import gleam/set
import gleam/yielder.{Done, Next, filter_map, find, unfold}

pub fn breadth_first_yielder(next: fn(a) -> List(a), initial: a) {
  unfold(from: #(deque.from_list([#(initial, [])]), set.new()), with: fn(state) {
    let #(queue, visited) = state

    let r = {
      use #(#(state, path), next_queue) <- try(pop_front(queue))

      let r = case set.contains(visited, state) {
        True -> Next(None, #(next_queue, visited))
        False -> {
          let next_visited = set.insert(visited, state)
          let next_path = [state, ..path]
          let next_states =
            state
            |> next()
            |> list.filter(fn(v) { !set.contains(next_visited, v) })
            |> list.map(fn(v) { #(v, next_path) })
          let next_queue =
            list.fold(next_states, next_queue, fn(acc, a) {
              deque.push_back(acc, a)
            })
          Next(Some(#(state, path)), #(next_queue, visited))
        }
      }
      Ok(r)
    }

    case r {
      Ok(a) -> a
      Error(_) -> Done
    }
  })
  |> filter_map(to_result(_, Nil))
}

pub fn breadth_first_search(
  next: fn(a) -> List(a),
  found: fn(a) -> Bool,
  initial: a,
) -> Result(#(a, List(a)), Nil) {
  let iterator = breadth_first_yielder(next, initial)
  find(iterator, fn(route) {
    let #(value, _) = route
    found(value)
  })
}
