import gleam/bool
import gleam/deque
import gleam/function
import gleam/list
import gleam/set
import gleam/yielder.{type Yielder}
import search_algorithms/utils

/// Create a Yielder iterating in breadth-first order, generating next states as states are visited
/// * States are not revisited
/// * Each yield is a `List(a)` representing the path from initial to current states in reverse order `[current, .., initial]`
pub fn breadth_first_yielder(
  next_states: fn(a) -> List(a),
  from initial: a,
) -> Yielder(List(a)) {
  yielder.unfold(
    from: #(deque.from_list([[initial]]), set.new()),
    with: fn(state) {
      let #(queue, visited) = state

      use #(path, next_queue) <- utils.unwrap_guard(
        deque.pop_front(queue),
        yielder.Done,
      )
      // use #(path, next_queue) <- deque.pop_front(queue)
      let assert [current, ..] = path

      use <- bool.guard(
        set.contains(visited, current),
        yielder.Next(Error(Nil), #(next_queue, visited)),
      )

      let next_visited = set.insert(visited, current)

      let ns =
        current
        |> next_states()
        |> list.filter(fn(v) { !set.contains(next_visited, v) })
        |> list.map(fn(v) { [v, ..path] })

      let next_queue = list.fold(ns, next_queue, deque.push_back)

      yielder.Next(Ok(path), #(next_queue, next_visited))
    },
  )
  |> yielder.filter_map(function.identity)
}

///
pub fn breadth_first_search(
  next: fn(a) -> List(a),
  found: fn(a) -> Bool,
  initial: a,
) -> Result(List(a), Nil) {
  let iterator = breadth_first_yielder(next, initial)
  yielder.find(iterator, fn(path) {
    let assert [value, ..] = path
    found(value)
  })
}
