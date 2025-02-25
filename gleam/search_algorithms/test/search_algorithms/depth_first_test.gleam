import gleam/result
import gleam/yielder
import gleeunit
import gleeunit/should
import non_empty_list
import search_algorithms/depth_first.{depth_first_search, depth_first_yielder}

pub fn main() {
  gleeunit.main()
}

pub type Node(a) {
  Node(a, List(Node(a)))
}

fn get_tree() {
  Node("1", [
    Node("11", [Node("111", []), Node("112", [])]),
    Node("12", [Node("121", []), Node("122", [])]),
  ])
}

pub fn depth_first_yielder_test() {
  let tree = get_tree()
  let next = fn(node: Node(String)) {
    let Node(_, children) = node
    children
  }

  let r =
    depth_first_yielder(next, tree)
    |> yielder.map(fn(path) {
      let state = non_empty_list.first(path)
      let Node(value, _) = state
      value
    })
    |> yielder.to_list

  r |> should.equal(["1", "11", "111", "112", "12", "121", "122"])
}

pub fn depth_first_search_test() {
  let tree = get_tree()
  let next = fn(node: Node(String)) {
    let Node(_, children) = node
    children
  }
  let found = fn(node: Node(String)) {
    let Node(val, _) = node
    val == "122"
  }

  let assert Ok(r) =
    depth_first_search(next, found, tree)
    |> result.map(fn(state) {
      non_empty_list.map(state, fn(node) {
        let Node(val, _) = node
        val
      })
    })

  r |> should.equal(non_empty_list.new("122", ["12", "1"]))
}
