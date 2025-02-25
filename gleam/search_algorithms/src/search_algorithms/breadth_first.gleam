import gleam/bool
import gleam/deque.{pop_front}
import gleam/function
import gleam/list
import gleam/result
import gleam/set
import gleam/yielder.{type Yielder, Done, Next, find, unfold}

/// Combings result.unwrap with bool.guard
/// Useful for extracting out an Ok() and returning early with handling Error() into something completely different
fn unwrap_guard(
  with result: Result(a, b),
  return consequence: fn(b) -> c,
  otherwise alternative: fn(a) -> c,
) -> c {
  bool.lazy_guard(
    result.is_error(result),
    fn() {
      let assert Error(b) = result
      consequence(b)
    },
    fn() {
      let assert Ok(a) = result
      alternative(a)
    },
  )
}

pub fn breadth_first_yielder(
  next: fn(a) -> List(a),
  initial: a,
) -> Yielder(List(a)) {
  unfold(from: #(deque.from_list([[initial]]), set.new()), with: fn(state) {
    let #(queue, visited) = state

    use #(path, next_queue) <- unwrap_guard(pop_front(queue), fn(_) { Done })
    let assert Ok(current) = list.first(path)

    use <- bool.guard(
      set.contains(visited, current),
      Next(Error(Nil), #(next_queue, visited)),
    )

    let next_visited = set.insert(visited, current)

    let next_states =
      current
      |> next()
      |> list.filter(fn(v) { !set.contains(next_visited, v) })
      |> list.map(fn(v) { [v, ..path] })

    let next_queue = list.fold(next_states, next_queue, deque.push_back)

    Next(Ok(path), #(next_queue, next_visited))
  })
  |> yielder.filter_map(function.identity)
}

pub fn breadth_first_search(
  next: fn(a) -> List(a),
  found: fn(a) -> Bool,
  initial: a,
) -> Result(List(a), Nil) {
  let iterator = breadth_first_yielder(next, initial)
  find(iterator, fn(path) {
    let assert Ok(value) = list.first(path)
    found(value)
  })
}
