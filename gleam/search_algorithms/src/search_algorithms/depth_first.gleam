import gleam/bool
import gleam/function
import gleam/list
import gleam/set
import gleam/yielder.{type Yielder}
import non_empty_list.{type NonEmptyList}
import search_algorithms/utils

pub fn depth_first_yielder(
  next: fn(a) -> List(a),
  initial: a,
) -> Yielder(NonEmptyList(a)) {
  let init_queue = non_empty_list.single(initial)
  yielder.unfold(from: #([init_queue], set.new()), with: fn(state) {
    let #(stack, visited) = state

    use #(path, next_stack) <- utils.uncons_guard(stack, yielder.Done)
    let current = non_empty_list.first(path)

    use <- bool.guard(
      set.contains(visited, current),
      yielder.Next(Error(Nil), #(next_stack, visited)),
    )

    let next_visited = set.insert(visited, current)

    let next_states =
      current
      |> next()
      |> list.filter(fn(v) { !set.contains(next_visited, v) })
      |> list.map(non_empty_list.prepend(path, _))

    let next_queue =
      list.fold_right(next_states, next_stack, fn(st, v) { [v, ..st] })

    yielder.Next(Ok(path), #(next_queue, next_visited))
  })
  |> yielder.filter_map(function.identity)
}

pub fn depth_first_search(
  next: fn(a) -> List(a),
  found: fn(a) -> Bool,
  initial: a,
) -> Result(NonEmptyList(a), Nil) {
  let iterator = depth_first_yielder(next, initial)
  yielder.find(iterator, fn(path) {
    let value = non_empty_list.first(path)
    found(value)
  })
}
