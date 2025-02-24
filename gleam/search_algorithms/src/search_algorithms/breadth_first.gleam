import gleam/bool
import gleam/deque.{pop_front}
import gleam/function
import gleam/list
import gleam/result
import gleam/set
import gleam/yielder.{type Yielder, Done, Next, find, unfold}

pub fn breadth_first_yielder(
  next: fn(a) -> List(a),
  initial: a,
) -> Yielder(List(a)) {
  unfold(from: #(deque.from_list([[initial]]), set.new()), with: fn(state) {
    let #(queue, visited) = state
    let front = pop_front(queue)

    use <- bool.guard(result.is_error(front), Done)

    let assert Ok(#(path, next_queue)) = front
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
