import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/set.{type Set}
import search_algorithms/internal/search_container.{type SearchContainer}

pub type SearchState(state_key, state) {
  SearchState(
    current: state,
    queue: SearchContainer(state),
    visited: Set(state_key),
    paths: Dict(state_key, List(state)),
  )
}

pub fn next_search_state(
  better: fn(List(state), List(state)) -> Bool,
  make_key: fn(state) -> state_key,
  next: fn(state) -> List(state),
  search_state prev: SearchState(state_key, state),
) -> Result(SearchState(state_key, state), Nil) {
  let update_queue_paths = fn(
    acc: #(SearchContainer(state), Dict(state_key, List(state))),
    state: state,
  ) {
    let key = make_key(state)
    let #(queue, paths) = acc
    case set.contains(prev.visited, key) {
      True -> acc
      False -> {
        let assert Ok(steps_so_far) =
          dict.get(prev.paths, make_key(prev.current))
        let q = search_container.push(queue, state)
        let ps = dict.insert(paths, key, [state, ..steps_so_far])

        case dict.get(paths, key) {
          Ok(old_path) -> {
            case better(old_path, [state, ..steps_so_far]) {
              True -> #(q, ps)
              False -> acc
            }
          }
          Error(_) -> #(q, ps)
        }
      }
    }
  }

  let #(new_queue, new_paths) =
    list.fold(next(prev.current), #(prev.queue, prev.paths), update_queue_paths)

  search_container.pop(new_queue)
  |> result.map(fn(q) {
    let #(new_current, remaining_queue) = q
    SearchState(
      new_current,
      remaining_queue,
      set.insert(prev.visited, make_key(new_current)),
      new_paths,
    )
  })
  |> result.then(fn(new_state) {
    case set.contains(prev.visited, make_key(new_state.current)) {
      True -> next_search_state(better, make_key, next, new_state)
      False -> Error(Nil)
    }
  })
}
