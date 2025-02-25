import common/list as listc
import gleam/bool
import gleam/function
import gleam/list
import gleam/set
import gleam/yielder.{type Yielder}
import non_empty_list.{type NonEmptyList}
import search_algorithms/generalized_search
import search_algorithms/internal/search_container.{Stack}

pub fn depth_first_yielder(
  next: fn(a) -> List(a),
  initial: a,
) -> Yielder(NonEmptyList(a)) {
  let init_path = non_empty_list.single(initial)
  yielder.unfold(from: #([init_path], set.new()), with: fn(state) {
    let #(stack, visited) = state

    use #(path, next_stack) <- listc.uncons_guard(stack, yielder.Done)
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

pub fn dfs(
  next: fn(a) -> List(a),
  found: fn(a) -> Bool,
  initial: a,
) -> Result(List(a), Nil) {
  generalized_search.generalized_search(
    Stack([]),
    function.identity,
    fn(_, _) { False },
    next,
    found,
    initial,
  )
}
