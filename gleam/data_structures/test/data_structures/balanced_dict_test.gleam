import common/list as listc
import data_structures/balanced_dict
import gleam/int
import gleam/list
import gleam/set
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

  dict
  |> balanced_dict.to_desc_list()
  |> list.map(fn(kvp) { kvp.0 })
  |> should.equal(ordered |> list.reverse())
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

pub fn has_key_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let dict =
    balanced_dict.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  list.each(ordered, fn(key) {
    balanced_dict.has_key(dict, key) |> should.equal(True)
  })

  balanced_dict.has_key(dict, 0) |> should.equal(False)
  balanced_dict.has_key(dict, 101) |> should.equal(False)
}

pub fn size_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let dict =
    balanced_dict.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  let re_shuffled = list.shuffle(ordered)
  let with_index = list.range(0, 99) |> list.reverse() |> list.zip(re_shuffled)

  list.fold(with_index, dict, fn(incoming, kvp) {
    let #(index, value) = kvp
    let outgoing = balanced_dict.delete(incoming, value)
    balanced_dict.size(outgoing) |> should.equal(index)
    outgoing
  })
}

pub fn map_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let dict =
    balanced_dict.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  balanced_dict.map_values(dict, fn(_, _) { "foo" })
  |> balanced_dict.to_asc_list()
  |> list.map(fn(kvp) { kvp.1 })
  |> should.equal(ordered |> list.map(fn(_) { "foo" }))
}

pub fn drop_test() {
  let ordered = list.range(1, 50)
  let shuffled = list.shuffle(ordered)
  let to_remove =
    list.filter(ordered, fn(_) {
      case int.random(1) {
        v if v > 0 -> True
        _ -> False
      }
    })
  let remaining =
    set.difference(set.from_list(ordered), set.from_list(to_remove))
    |> set.to_list()
    |> list.sort(int.compare)

  let dict =
    balanced_dict.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  balanced_dict.drop(
    dict,
    dict |> balanced_dict.to_asc_list() |> list.map(fn(kvp) { kvp.0 }),
  )
  |> should.equal(balanced_dict.new(int.compare))

  let updated = balanced_dict.drop(dict, to_remove)
  balanced_dict.to_asc_list(updated)
  |> list.map(fn(kvp) { kvp.0 })
  |> should.equal(remaining)
}

pub fn take_test() {
  let ordered = list.range(1, 50)
  let shuffled = list.shuffle(ordered)
  let to_keep =
    list.filter(ordered, fn(_) {
      case int.random(1) {
        v if v > 0 -> True
        _ -> False
      }
    })

  let dict =
    balanced_dict.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  balanced_dict.take(dict, []) |> should.equal(balanced_dict.new(int.compare))

  let updated = balanced_dict.take(dict, to_keep)
  balanced_dict.to_asc_list(updated)
  |> list.map(fn(kvp) { kvp.0 })
  |> should.equal(to_keep |> list.sort(int.compare))
}

pub fn get_test() {
  let ordered = list.range(1, 100)
  let shuffled1 = list.shuffle(ordered)
  let shuffled2 = list.shuffle(ordered)
  let tuples = list.zip(shuffled1, shuffled2)

  let dict = balanced_dict.from_list(tuples, int.compare)

  list.each(list.shuffle(ordered), fn(k) {
    let assert Ok(v1) = list.key_find(tuples, k)
    let assert Ok(v2) = balanced_dict.get(dict, k)
    v1 |> should.equal(v2)
  })
}

pub fn view_min_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let assert [_, ..to_assert] = ordered

  let dict =
    balanced_dict.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  let assert Ok(#(kvp, next)) = balanced_dict.view_min(dict)
  kvp.0 |> should.equal(1)
  balanced_dict.to_asc_list(next)
  |> list.map(fn(kvp) { kvp.0 })
  |> should.equal(to_assert)
}

pub fn view_max_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let assert Ok(to_assert) = listc.init(ordered)

  let dict =
    balanced_dict.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  let assert Ok(#(kvp, next)) = balanced_dict.view_max(dict)
  kvp.0 |> should.equal(100)
  balanced_dict.to_asc_list(next)
  |> list.map(fn(kvp) { kvp.0 })
  |> should.equal(to_assert)
}

pub fn get_min_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let dict =
    balanced_dict.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  let assert Ok(kvp) = balanced_dict.get_min(dict)
  kvp |> should.equal(#(1, ""))
}

pub fn get_max_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let dict =
    balanced_dict.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  let assert Ok(kvp) = balanced_dict.get_max(dict)
  kvp |> should.equal(#(100, ""))
}
