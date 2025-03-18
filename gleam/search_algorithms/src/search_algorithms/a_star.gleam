import common/result as resultc
import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/yielder.{type Yielder}
import search_algorithms/internal/priority_queue.{type Queue}

fn create_path_loop(came_from: Dict(a, a), state: a, acc: List(a)) {
  case dict.get(came_from, state) {
    Error(_) -> list.reverse(acc)
    Ok(prev) -> {
      io.debug(prev)
      create_path_loop(came_from, prev, [prev, ..acc])
    }
  }
}

fn create_path(came_from: Dict(a, a), state: a) -> List(a) {
  create_path_loop(came_from, state, [state])
}

pub type SearchState(a) {
  SearchState(
    came_from: Dict(a, a),
    g: Dict(a, Float),
    f: Dict(a, Float),
    queue: Queue(a),
  )
}

fn make_compare(f: Dict(a, Float)) {
  fn(a, b) {
    let assert Ok(a_score) = dict.get(f, a)
    let assert Ok(b_score) = dict.get(f, b)
    case a_score <. b_score {
      True -> Lt
      False -> Gt
    }
  }
}

pub fn a_star_assoc_yielder(
  next: fn(a) -> List(#(a, Float)),
  heuristic: fn(a) -> Float,
  from initial: a,
) -> Yielder(#(Float, List(a))) {
  let g = dict.from_list([#(initial, 0.0)])
  let f = dict.from_list([#(initial, heuristic(initial))])

  let queue =
    priority_queue.new(make_compare(f)) |> priority_queue.push(initial)

  let init_state = SearchState(dict.new(), g, f, queue)

  yielder.unfold(from: init_state, with: fn(search_state) {
    let SearchState(came_from, _, g, queue) = search_state

    use #(state, next_queue) <- resultc.unwrap_guard(
      priority_queue.pop(queue),
      yielder.Done,
    )

    let assert Ok(cost) = dict.get(g, state)

    io.debug(state)
    let to_yield = #(cost, create_path(came_from, state))

    let next_states = next(state)
    io.debug(next_states)

    let next_search_state =
      list.fold(
        next_states,
        SearchState(..search_state, queue: next_queue),
        fn(search_state, t) {
          let #(next_state, next_cost) = t
          let SearchState(came_from, f, g, queue) = search_state
          let tentative_g_score = cost +. next_cost

          // unwrap(True) is because if dict.get() is Error,
          // because that means the tentative_g_score is lower because we don't yet have a g_score
          let has_better_score =
            dict.get(g, next_state)
            |> result.map(fn(g_val) { tentative_g_score <. g_val })
            |> result.unwrap(True)

          case has_better_score {
            False -> search_state
            True -> {
              let next_came_from = dict.insert(came_from, next_state, state)
              let next_g = dict.insert(g, next_state, tentative_g_score)
              let next_f =
                dict.insert(
                  f,
                  next_state,
                  tentative_g_score +. heuristic(next_state),
                )
              let next_queue =
                queue
                |> priority_queue.reorder(make_compare(next_f))
                |> priority_queue.push(next_state)

              // next_queue |> priority_queue.to_list |> io.debug

              SearchState(next_came_from, next_g, next_f, next_queue)
            }
          }
        },
      )

    yielder.Next(to_yield, next_search_state)
  })
}

pub fn a_star_yielder(
  next: fn(a) -> List(a),
  get_cost: fn(a, a) -> Float,
  heuristic: fn(a) -> Float,
  from initial: a,
) -> Yielder(#(Float, List(a))) {
  let next_assoc = fn(state) {
    next(state) |> list.map(fn(n) { #(n, get_cost(state, n)) })
  }
  a_star_assoc_yielder(next_assoc, heuristic, initial)
}

pub fn a_star_assoc(
  next: fn(a) -> List(#(a, Float)),
  heuristic: fn(a) -> Float,
  found: fn(a) -> Bool,
  from initial: a,
) -> Result(#(Float, List(a)), Nil) {
  yielder.fold_until(
    a_star_assoc_yielder(next, heuristic, initial),
    Error(Nil),
    fn(acc, state) {
      let #(_, path) = state
      let assert [current] = path
      case found(current) {
        True -> list.Stop(Ok(state))
        False -> list.Continue(acc)
      }
    },
  )
}

pub fn a_star(
  next: fn(a) -> List(a),
  get_cost: fn(a, a) -> Float,
  heuristic: fn(a) -> Float,
  found: fn(a) -> Bool,
  from initial: a,
) -> Result(#(Float, List(a)), Nil) {
  yielder.fold_until(
    a_star_yielder(next, get_cost, heuristic, initial),
    Error(Nil),
    fn(acc, state) {
      let #(_, path) = state
      let assert [current, ..] = path
      case found(current) {
        True -> list.Stop(Ok(state))
        False -> list.Continue(acc)
      }
    },
  )
}
