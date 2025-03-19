import common/list as listc
import data_structures/balanced_set
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

  let set = balanced_set.from_list(shuffled, int.compare)

  set
  |> balanced_set.to_asc_list()
  |> should.equal(ordered)

  set
  |> balanced_set.to_desc_list()
  |> should.equal(ordered |> list.reverse())
}

pub fn overwrites_existing_keys_test() {
  let set =
    balanced_set.new(string.compare)
    |> balanced_set.insert("foo")
    |> balanced_set.insert("foo")
    |> balanced_set.insert("foo")

  balanced_set.to_asc_list(set) |> should.equal(["foo"])
}

pub fn contains_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let set = balanced_set.from_list(shuffled, int.compare)

  list.each(ordered, fn(key) {
    balanced_set.contains(set, key) |> should.equal(True)
  })

  balanced_set.contains(set, 0) |> should.equal(False)
  balanced_set.contains(set, 101) |> should.equal(False)
}

pub fn size_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let dict = balanced_set.from_list(shuffled, int.compare)

  let re_shuffled = list.shuffle(ordered)
  let with_index = list.range(0, 99) |> list.reverse() |> list.zip(re_shuffled)

  list.fold(with_index, dict, fn(incoming, kvp) {
    let #(index, value) = kvp
    let outgoing = balanced_set.delete(incoming, value)
    balanced_set.size(outgoing) |> should.equal(index)
    outgoing
  })
}

pub fn map_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let set = balanced_set.from_list(shuffled, int.compare)

  balanced_set.map(set, int.compare, fn(x) { x * 2 })
  |> balanced_set.to_asc_list()
  |> should.equal(ordered |> list.map(fn(x) { x * 2 }))
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

  let set = balanced_set.from_list(shuffled, int.compare)

  balanced_set.drop(set, set |> balanced_set.to_asc_list())
  |> should.equal(balanced_set.new(int.compare))

  let updated = balanced_set.drop(set, to_remove)
  balanced_set.to_asc_list(updated)
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

  let set = balanced_set.from_list(shuffled, int.compare)

  balanced_set.take(set, []) |> should.equal(balanced_set.new(int.compare))

  let updated = balanced_set.take(set, to_keep)
  balanced_set.to_asc_list(updated)
  |> should.equal(to_keep |> list.sort(int.compare))
}

pub fn view_min_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let assert [_, ..to_assert] = ordered

  let dict = balanced_set.from_list(shuffled, int.compare)

  let assert Ok(#(v, next)) = balanced_set.view_min(dict)
  v |> should.equal(1)

  balanced_set.to_asc_list(next)
  |> should.equal(to_assert)
}

pub fn view_max_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let assert Ok(to_assert) = listc.init(ordered)

  let set = balanced_set.from_list(shuffled, int.compare)

  let assert Ok(#(v, next)) = balanced_set.view_max(set)
  v |> should.equal(100)

  balanced_set.to_asc_list(next)
  |> should.equal(to_assert)
}

pub fn get_min_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let set = balanced_set.from_list(shuffled, int.compare)

  let assert Ok(v) = balanced_set.get_min(set)
  v |> should.equal(1)
}

pub fn get_max_test() {
  let ordered = list.range(1, 100)
  let shuffled = list.shuffle(ordered)

  let set = balanced_set.from_list(shuffled, int.compare)

  let assert Ok(v) = balanced_set.get_max(set)
  v |> should.equal(100)
}

pub fn difference_test() {
  let a = balanced_set.from_list(list.range(1, 10), int.compare)
  let b = balanced_set.from_list(list.range(11, 20), int.compare)
  let c = balanced_set.from_list(list.range(1, 20), int.compare)

  balanced_set.difference(a, b)
  |> balanced_set.to_asc_list()
  |> should.equal(a |> balanced_set.to_asc_list())
  balanced_set.difference(a, c)
  |> balanced_set.to_asc_list()
  |> should.equal(balanced_set.new(int.compare) |> balanced_set.to_asc_list())
  balanced_set.difference(b, c)
  |> balanced_set.to_asc_list()
  |> should.equal(balanced_set.new(int.compare) |> balanced_set.to_asc_list())
}

pub fn intersection_test() {
  let a = balanced_set.from_list(list.range(1, 10), int.compare)
  let b = balanced_set.from_list(list.range(11, 20), int.compare)
  let c = balanced_set.from_list(list.range(1, 20), int.compare)

  balanced_set.intersection(a, b)
  |> balanced_set.to_asc_list()
  |> should.equal(balanced_set.new(int.compare) |> balanced_set.to_asc_list())
  balanced_set.intersection(a, c)
  |> balanced_set.to_asc_list()
  |> should.equal(a |> balanced_set.to_asc_list())
  balanced_set.intersection(b, c)
  |> balanced_set.to_asc_list()
  |> should.equal(b |> balanced_set.to_asc_list())
}

pub fn is_disjoint_test() {
  let a = balanced_set.from_list(list.range(1, 10), int.compare)
  let b = balanced_set.from_list(list.range(11, 20), int.compare)
  let c = balanced_set.from_list(list.range(1, 20), int.compare)

  balanced_set.is_disjoint(a, b)
  |> should.be_true()
  balanced_set.is_disjoint(a, c)
  |> should.be_false()
  balanced_set.is_disjoint(b, c)
  |> should.be_false()
}

pub fn is_empty_test() {
  balanced_set.new(int.compare) |> balanced_set.is_empty() |> should.be_true()
  balanced_set.from_list([], int.compare)
  |> balanced_set.is_empty()
  |> should.be_true()
  balanced_set.from_list([1], int.compare)
  |> balanced_set.is_empty()
  |> should.be_false()
}

pub fn is_subset_test() {
  let a = balanced_set.from_list(list.range(1, 10), int.compare)
  let b = balanced_set.from_list(list.range(11, 20), int.compare)
  let c = balanced_set.from_list(list.range(1, 20), int.compare)

  balanced_set.is_subset(a, b)
  |> should.be_false()
  balanced_set.is_subset(a, c)
  |> should.be_true()
  balanced_set.is_subset(b, c)
  |> should.be_true()
}

pub fn symmetric_difference_test() {
  let a = balanced_set.from_list(list.range(1, 10), int.compare)
  let b = balanced_set.from_list(list.range(11, 20), int.compare)
  let c = balanced_set.from_list(list.range(1, 20), int.compare)

  balanced_set.symmetric_difference(a, b)
  |> balanced_set.to_asc_list()
  |> should.equal(c |> balanced_set.to_asc_list())
  balanced_set.symmetric_difference(a, c)
  |> balanced_set.to_asc_list()
  |> should.equal(b |> balanced_set.to_asc_list())
  balanced_set.symmetric_difference(b, c)
  |> balanced_set.to_asc_list()
  |> should.equal(a |> balanced_set.to_asc_list())
}

pub fn union_test() {
  let a = balanced_set.from_list(list.range(1, 10), int.compare)
  let b = balanced_set.from_list(list.range(11, 20), int.compare)
  let c = balanced_set.from_list(list.range(1, 20), int.compare)

  balanced_set.union(a, b)
  |> balanced_set.to_asc_list()
  |> should.equal(c |> balanced_set.to_asc_list())
  balanced_set.union(a, c)
  |> balanced_set.to_asc_list()
  |> should.equal(c |> balanced_set.to_asc_list())
  balanced_set.union(b, c)
  |> balanced_set.to_asc_list()
  |> should.equal(c |> balanced_set.to_asc_list())
}
