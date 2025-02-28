import gleeunit
import gleeunit/should
import search_algorithms/generalized_search.{find_iterate}

pub fn main() {
  gleeunit.main()
}

pub fn next_search_state() {
  1 |> should.equal(1)
}
