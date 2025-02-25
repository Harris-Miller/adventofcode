import common/list as listc
import gleam/deque.{type Deque}
import gleam/list
import gleam/option
import gleam/result

pub type SearchContainer(a) {
  Stack(List(a))
  Queue(Deque(a))
}

pub fn pop(sc: SearchContainer(a)) -> Result(#(a, SearchContainer(a)), Nil) {
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

pub fn push(sc: SearchContainer(a), item: a) -> SearchContainer(a) {
  case sc {
    Stack(list) -> list.prepend(list, item) |> Stack()
    Queue(queue) -> deque.push_back(queue, item) |> Queue()
  }
}
