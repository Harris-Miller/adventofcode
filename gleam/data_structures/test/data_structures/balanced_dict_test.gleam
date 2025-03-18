import data_structures/balanced_dict
import gleam/int
import gleam/io
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
  io.debug(shuffled)

  let dict =
    balanced_dict.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  // io.debug(balanced_dict.size(dict))
  // io.debug(balanced_dict.to_asc_list(dict))

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
