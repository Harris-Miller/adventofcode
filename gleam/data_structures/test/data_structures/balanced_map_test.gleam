import common/list as listc
import data_structures/balanced_map
import gleam/int
import gleam/list
import gleam/pair
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

  let map =
    balanced_map.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  map
  |> balanced_map.to_asc_list()
  |> list.map(pair.first)
  |> should.equal(ordered)

  map
  |> balanced_map.to_desc_list()
  |> list.map(pair.first)
  |> should.equal(ordered |> list.reverse())
}

pub fn overwrites_existing_keys_test() {
  let map =
    balanced_map.new(string.compare)
    |> balanced_map.insert("foo", 1)
    |> balanced_map.insert("foo", 3)
    |> balanced_map.insert("foo", 5)
  let assert Ok(v) = balanced_map.get(map, "foo")
  v |> should.equal(5)
}

pub fn has_key_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let map =
    balanced_map.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  list.each(ordered, fn(key) {
    balanced_map.has_key(map, key) |> should.equal(True)
  })

  balanced_map.has_key(map, 0) |> should.equal(False)
  balanced_map.has_key(map, 101) |> should.equal(False)
}

pub fn size_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let map =
    balanced_map.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  let re_shuffled = list.shuffle(ordered)
  let with_index = list.range(0, 99) |> list.reverse() |> list.zip(re_shuffled)

  list.fold(with_index, map, fn(incoming, kvp) {
    let #(index, value) = kvp
    let outgoing = balanced_map.delete(incoming, value)
    balanced_map.size(outgoing) |> should.equal(index)
    outgoing
  })
}

pub fn map_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let map =
    balanced_map.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  balanced_map.map_values(map, fn(_, _) { "foo" })
  |> balanced_map.to_asc_list()
  |> list.map(pair.second)
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

  let map =
    balanced_map.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  balanced_map.drop(
    map,
    map |> balanced_map.to_asc_list() |> list.map(pair.first),
  )
  |> should.equal(balanced_map.new(int.compare))

  let updated = balanced_map.drop(map, to_remove)
  balanced_map.to_asc_list(updated)
  |> list.map(pair.first)
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

  let map =
    balanced_map.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  balanced_map.take(map, []) |> should.equal(balanced_map.new(int.compare))

  let updated = balanced_map.take(map, to_keep)
  balanced_map.to_asc_list(updated)
  |> list.map(pair.first)
  |> should.equal(to_keep |> list.sort(int.compare))
}

pub fn get_test() {
  let ordered = list.range(1, 100)
  let shuffled1 = list.shuffle(ordered)
  let shuffled2 = list.shuffle(ordered)
  let tuples = list.zip(shuffled1, shuffled2)

  let map = balanced_map.from_list(tuples, int.compare)

  list.each(list.shuffle(ordered), fn(k) {
    let assert Ok(v1) = list.key_find(tuples, k)
    let assert Ok(v2) = balanced_map.get(map, k)
    v1 |> should.equal(v2)
  })
}

pub fn view_min_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let assert [_, ..to_assert] = ordered

  let map =
    balanced_map.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  let assert Ok(#(kvp, next)) = balanced_map.view_min(map)
  kvp.0 |> should.equal(1)
  balanced_map.to_asc_list(next)
  |> list.map(pair.first)
  |> should.equal(to_assert)
}

pub fn view_max_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let assert Ok(to_assert) = listc.init(ordered)

  let map =
    balanced_map.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  let assert Ok(#(kvp, next)) = balanced_map.view_max(map)
  kvp.0 |> should.equal(100)
  balanced_map.to_asc_list(next)
  |> list.map(pair.first)
  |> should.equal(to_assert)
}

pub fn get_min_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let map =
    balanced_map.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  let assert Ok(kvp) = balanced_map.get_min(map)
  kvp |> should.equal(#(1, ""))
}

pub fn get_max_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let map =
    balanced_map.from_list(list.map(shuffled, fn(k) { #(k, "") }), int.compare)

  let assert Ok(kvp) = balanced_map.get_max(map)
  kvp |> should.equal(#(100, ""))
}
