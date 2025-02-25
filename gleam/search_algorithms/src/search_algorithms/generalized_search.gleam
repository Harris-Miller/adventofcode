import common/result as resultc
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

// fn least_costly(a: List(#(a, b)), b: List(#(a, b))) {
//   case a, b {
//     [#(cost_a, _), ..], [#(cost_b, _), ..] -> True
//     // logically this never happens, but need to exhaust the pattern matching
//     [], _ -> False
//     _, [] -> True
//   }
// }

fn find_iterate(
  next: fn(a) -> Result(a, Nil),
  found: fn(a) -> Bool,
  initial: a,
) -> Result(a, Nil) {
  let is_found = found(initial)
  case is_found {
    True -> Ok(initial)
    False -> next(initial) |> result.then(find_iterate(next, found, _))
  }
}

fn next_search_state(
  better: fn(List(state), List(state)) -> Bool,
  make_key: fn(state) -> state_key,
  next: fn(state) -> List(state),
  old: SearchState(state_key, state),
) -> Result(SearchState(state_key, state), Nil) {
  let update_queue_paths = fn(
    olds: #(SearchContainer(state), Dict(state_key, List(state))),
    st: state,
  ) {
    let key = make_key(st)
    let #(old_queue, old_paths) = olds
    case set.contains(old.visited, key) {
      True -> olds
      False -> {
        let steps_so_far =
          dict.get(old.paths, make_key(old.current))
          |> resultc.unwrap_assert()
        let q = search_container.push(old_queue, st)
        let ps = dict.insert(old_paths, key, [st, ..steps_so_far])

        case dict.get(old_paths, key) {
          Ok(old_path) -> {
            case better(old_path, [st, ..steps_so_far]) {
              True -> #(q, ps)
              False -> olds
            }
          }
          Error(_) -> #(q, ps)
        }
      }
    }
  }

  let #(new_queue, new_paths) =
    list.fold(next(old.current), #(old.queue, old.paths), update_queue_paths)

  search_container.pop(new_queue)
  |> result.map(fn(q) {
    let #(new_current, remaining_queue) = q
    SearchState(
      new_current,
      remaining_queue,
      set.insert(old.visited, make_key(new_current)),
      new_paths,
    )
  })
  |> result.then(fn(new_state) {
    case set.contains(old.visited, make_key(new_state.current)) {
      True -> next_search_state(better, make_key, next, new_state)
      False -> Error(Nil)
    }
  })
}

pub fn generalized_search(
  container: SearchContainer(state),
  make_key: fn(state) -> state_key,
  better: fn(List(state), List(state)) -> Bool,
  next: fn(state) -> List(state),
  found: fn(state) -> Bool,
  initial: state,
) -> Result(List(state), Nil) {
  let initial_key = make_key(initial)
  let initial_state =
    SearchState(
      initial,
      container,
      set.from_list([initial_key]),
      dict.from_list([#(initial_key, [])]),
    )

  let end_result =
    find_iterate(
      next_search_state(better, make_key, next, _),
      fn(st) { found(st.current) },
      initial_state,
    )

  let get_steps = fn(search_st: SearchState(state_key, state)) {
    dict.get(search_st.paths, make_key(search_st.current))
    |> resultc.unwrap_assert()
  }

  result.map(end_result, fn(st) { st |> get_steps() |> list.reverse() })
}
