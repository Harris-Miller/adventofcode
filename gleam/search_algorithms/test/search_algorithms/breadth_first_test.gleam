import gleam/list
import gleam/yielder
import gleeunit
import gleeunit/should
import search_algorithms/breadth_first

pub fn main() {
  gleeunit.main()
}

pub fn breadth_first_yielder_test() {
  let next = fn(value: String) { list.map(["1", "2"], fn(v) { value <> v }) }

  let r =
    breadth_first.breadth_first_yielder(next, "1")
    |> yielder.take_while(satisfying: fn(state) {
      let #(val, _) = state
      val != "1111"
    })
    |> yielder.map(fn(state) { state.0 })
    |> yielder.to_list

  r
  |> should.equal(["1", "11", "12", "111", "112", "121", "122"])
}
