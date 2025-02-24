import gleam/list
import gleam/yielder
import gleeunit
import gleeunit/should
import search_algorithms/breadth_first.{
  breadth_first_search, breadth_first_yielder,
}

pub fn main() {
  gleeunit.main()
}

pub fn breadth_first_yielder_test() {
  let next = fn(value: String) { list.map(["1", "2"], fn(v) { value <> v }) }

  let r =
    breadth_first_yielder(next, "1")
    |> yielder.take_while(satisfying: fn(state) {
      let assert Ok(val) = list.first(state)
      val != "1111"
    })
    |> yielder.map(fn(state) {
      let assert Ok(val) = list.first(state)
      val
    })
    |> yielder.to_list

  r |> should.equal(["1", "11", "12", "111", "112", "121", "122"])
}

pub fn breadth_first_search_test() {
  let next = fn(value: String) { list.map(["1", "2"], fn(v) { value <> v }) }
  let found = fn(value: String) { value == "122" }

  let results = breadth_first_search(next, found, "1")

  results
  |> should.equal(Ok(["122", "12", "1"]))
}
