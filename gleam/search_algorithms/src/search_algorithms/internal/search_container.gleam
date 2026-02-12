import data_structures/balanced_map.{type BalancedMap}
import gleam/deque.{type Deque}

import gleam/list
import gleam/result

pub type Type(a) {
  One(a)
  Two(Int, a)
}

pub type SearchContainer(a) {
  Stack(List(a))
  Queue(Deque(a))
  LIFOHeap(BalancedMap(Int, List(a)))
}

pub fn pop(sc: SearchContainer(a)) -> Result(#(a, SearchContainer(a)), Nil) {
  case sc {
    Stack(list) -> {
      case list {
        [head, ..tail] -> Ok(#(head, Stack(tail)))
        [] -> Error(Nil)
      }
    }
    Queue(queue) ->
      deque.pop_front(queue)
      |> result.map(fn(t) {
        let #(a, queue) = t
        #(a, Queue(queue))
      })
    LIFOHeap(map) -> {
      case balanced_map.get_min(map) {
        Error(Nil) -> Error(Nil)
        Ok(#(k, [v])) -> Ok(#(v, LIFOHeap(balanced_map.delete(map, k))))
        Ok(#(k, [v, ..rest])) ->
          Ok(#(v, LIFOHeap(balanced_map.insert(map, k, rest))))
        Ok(#(_, [])) -> {
          let assert Ok(#(_, m)) = balanced_map.view_min(map)
          pop(LIFOHeap(m))
        }
      }
    }
  }
}

pub fn push(sc: SearchContainer(a), value: a) -> SearchContainer(a) {
  case sc {
    Stack(list) -> value |> list.prepend(list, _) |> Stack()
    Queue(queue) -> value |> deque.push_back(queue, _) |> Queue()
    LIFOHeap(map) -> {
      let assert #(cost, _) = value
      balanced_map.upsert(map, cost, fn(opt) {
        case opt {
          None -> [value]
          Some(list) -> [value, ..list]
        }
      })
      |> LIFOHeap()
    }
  }
}
