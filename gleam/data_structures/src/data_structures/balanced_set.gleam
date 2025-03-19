import data_structures/balanced_dict.{type BalancedDict}
import gleam/list
import gleam/order.{type Order}
import gleam/result

pub type BalancedSet(a) =
  BalancedDict(a, Nil)

pub fn clear(set: BalancedSet(a)) -> BalancedSet(a) {
  balanced_dict.clear(set)
}

pub fn contains(in set: BalancedSet(a), this member: a) -> Bool {
  balanced_dict.has_key(set, member)
}

pub fn delete(from set: BalancedSet(a), this member: a) -> BalancedSet(a) {
  balanced_dict.delete(set, member)
}

pub fn difference() {
  todo
}

pub fn drop(
  from set: BalancedSet(a),
  drop disallowed: List(a),
) -> BalancedSet(a) {
  balanced_dict.drop(set, disallowed)
}

pub fn each(set: BalancedSet(a), fun: fn(a) -> b) -> Nil {
  fold(set, Nil, fn(nil, k) {
    fun(k)
    nil
  })
}

pub fn filter(
  in set: BalancedSet(a),
  keeping predicate: fn(a) -> Bool,
) -> BalancedSet(a) {
  balanced_dict.filter(set, fn(k, _) { predicate(k) })
}

pub fn fold(
  over set: BalancedSet(a),
  from initial: b,
  with reducer: fn(b, a) -> b,
) -> b {
  balanced_dict.fold(set, initial, fn(acc, k, _) { reducer(acc, k) })
}

pub fn fold_right(
  over set: BalancedSet(a),
  from initial: b,
  with reducer: fn(b, a) -> b,
) -> b {
  balanced_dict.fold_right(set, initial, fn(acc, k, _) { reducer(acc, k) })
}

pub fn from_list(members: List(a), compare: fn(a, a) -> Order) -> BalancedSet(a) {
  list.fold(members, new(compare), fn(acc, key) {
    balanced_dict.insert(acc, key, Nil)
  })
}

pub fn get_min(from set: BalancedSet(a)) -> Result(a, Nil) {
  set |> balanced_dict.get_min() |> result.map(fn(kvp) { kvp.0 })
}

pub fn get_max(from set: BalancedSet(a)) -> Result(a, Nil) {
  set |> balanced_dict.get_max() |> result.map(fn(kvp) { kvp.0 })
}

pub fn insert(into set: BalancedSet(a), this member: a) -> BalancedSet(a) {
  balanced_dict.insert(set, member, Nil)
}

pub fn intersection(
  of first: BalancedSet(a),
  and second: BalancedSet(a),
) -> BalancedSet(a) {
  todo
}

pub fn is_disjoint(first: BalancedSet(a), from second: BalancedSet(a)) -> Bool {
  todo
}

pub fn is_empty(set: BalancedSet(a)) -> Bool {
  balanced_dict.size(set) == 0
}

pub fn is_subset(first: BalancedSet(a), of second: BalancedSet(a)) -> Bool {
  todo
}

pub fn map(set: BalancedSet(a), with fun: fn(a) -> b) -> BalancedSet(b) {
  todo
}

pub fn new(compare: fn(v, v) -> Order) -> BalancedSet(v) {
  balanced_dict.new(compare)
}

pub fn size(set: BalancedSet(v)) -> Int {
  balanced_dict.size(set)
}

pub fn symmetric_difference(
  of first: BalancedSet(a),
  and second: BalancedSet(a),
) -> BalancedSet(a) {
  todo
}

pub fn take(
  from set: BalancedSet(a),
  keeping desired: List(a),
) -> BalancedSet(a) {
  balanced_dict.take(set, desired)
}

pub fn to_asc_list(set: BalancedSet(a)) -> List(a) {
  balanced_dict.to_asc_list(set) |> list.map(fn(kvp) { kvp.0 })
}

pub fn to_desc_list(set: BalancedSet(a)) -> List(a) {
  balanced_dict.to_desc_list(set) |> list.map(fn(kvp) { kvp.0 })
}

pub fn view_min(from set: BalancedSet(a)) -> Result(#(a, BalancedSet(a)), Nil) {
  set
  |> balanced_dict.view_min()
  |> result.map(fn(r) {
    let #(kvp, new_set) = r
    #(kvp.0, new_set)
  })
}

pub fn view_max(from set: BalancedSet(a)) -> Result(#(a, BalancedSet(a)), Nil) {
  set
  |> balanced_dict.view_max()
  |> result.map(fn(r) {
    let #(kvp, new_set) = r
    #(kvp.0, new_set)
  })
}
