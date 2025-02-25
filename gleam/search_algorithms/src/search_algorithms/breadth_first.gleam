import common/result as resultc
import gleam/bool
import gleam/deque
import gleam/function
import gleam/list
import gleam/set
import gleam/yielder.{type Yielder}
import non_empty_list.{type NonEmptyList}

/// Create a Yielder iterating in breadth-first order, generating next states as states are visited
/// * States are not revisited
/// * Each yield is a `List(a)` representing the path from initial to current states in reverse order `[current, .., initial]`
pub fn breadth_first_yielder(
  next_states: fn(a) -> List(a),
  from initial: a,
) -> Yielder(NonEmptyList(a)) {
  let init_path = non_empty_list.single(initial)
  yielder.unfold(
    from: #(deque.from_list([init_path]), set.new()),
    with: fn(state) {
      let #(queue, visited) = state

      use #(path, next_queue) <- resultc.unwrap_guard(
        deque.pop_front(queue),
        yielder.Done,
      )
      let current = non_empty_list.first(path)
      // let assert [current, ..] = path

      use <- bool.guard(
        set.contains(visited, current),
        yielder.Next(Error(Nil), #(next_queue, visited)),
      )

      let next_visited = set.insert(visited, current)

      let ns =
        current
        |> next_states()
        |> list.filter(fn(v) { !set.contains(next_visited, v) })
        |> list.map(non_empty_list.prepend(path, _))

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
) -> Result(NonEmptyList(a), Nil) {
  let iterator = breadth_first_yielder(next, initial)
  yielder.find(iterator, fn(path) {
    let value = non_empty_list.first(path)
    found(value)
  })
}
