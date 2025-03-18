import data_structures/balanced_dict
import gleam/int
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn keeps_everything_in_order_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let dict =
    balanced_dict.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  dict
  |> balanced_dict.to_asc_list()
  |> list.map(fn(kvp) { kvp.0 })
  |> should.equal(ordered)
}

pub fn overwrites_existing_keys_test() {
  let dict =
    balanced_dict.new(string.compare)
    |> balanced_dict.insert("foo", 1)
    |> balanced_dict.insert("foo", 3)
    |> balanced_dict.insert("foo", 5)
  let assert Ok(v) = balanced_dict.get(dict, "foo")
  v |> should.equal(5)
}

pub fn size_test() {
  let rand = case int.random(100) {
    0 -> 1
    a -> a
  }
  let ordered = list.range(1, rand)

  let dict =
    balanced_dict.from_list(list.map(ordered, fn(k) { #(k, "") }), int.compare)

  dict |> balanced_dict.size() |> should.equal(rand)
}
