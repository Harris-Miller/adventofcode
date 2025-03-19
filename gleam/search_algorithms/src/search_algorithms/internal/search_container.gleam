import common/list as listc
import data_structures/balanced_map.{type BalancedMap}
import gleam/deque.{type Deque}
import gleam/list
import gleam/option
import gleam/result

pub type SearchContainer(k, v) {
  Stack(List(k))
  Queue(Deque(k))
  // LIFOHeap(BalancedMap(k, v))
}

pub fn pop(
  sc: SearchContainer(k, v),
) -> Result(#(k, SearchContainer(k, v)), Nil) {
  case sc {
    Stack(list) ->
      listc.uncons(list)
      |> option.to_result(Nil)
      |> result.map(fn(t) {
        let #(a, list) = t
        #(a, Stack(list))
      })
    Queue(queue) ->
      deque.pop_front(queue)
      |> result.map(fn(t) {
        let #(a, queue) = t
        #(a, Queue(queue))
      })
  }
}

pub fn push(sc: SearchContainer(k, v), item: k) -> SearchContainer(k, v) {
  case sc {
    Stack(list) -> list.prepend(list, item) |> Stack()
    Queue(queue) -> deque.push_back(queue, item) |> Queue()
  }
}
