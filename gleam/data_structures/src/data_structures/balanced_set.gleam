import data_structures/balanced_dict.{type BalancedDict}
import gleam/list
import gleam/order.{type Order}
import gleam/result

pub opaque type BalancedSet(a) {
  BalancedSet(dict: BalancedDict(a, Nil), compare: fn(a, a) -> Order)
}

pub fn clear(set: BalancedSet(a)) -> BalancedSet(a) {
  BalancedSet(..set, dict: balanced_dict.clear(set.dict))
}

pub fn contains(in set: BalancedSet(a), this member: a) -> Bool {
  balanced_dict.has_key(set.dict, member)
}

pub fn delete(from set: BalancedSet(a), this member: a) -> BalancedSet(a) {
  BalancedSet(..set, dict: balanced_dict.delete(set.dict, member))
}

pub fn difference(
  from first: BalancedSet(a),
  minus second: BalancedSet(a),
) -> BalancedSet(a) {
  drop(from: first, drop: to_asc_list(second))
}

pub fn drop(
  from set: BalancedSet(a),
  drop disallowed: List(a),
) -> BalancedSet(a) {
  BalancedSet(..set, dict: balanced_dict.drop(set.dict, disallowed))
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
  BalancedSet(
    ..set,
    dict: balanced_dict.filter(set.dict, fn(k, _) { predicate(k) }),
  )
}

pub fn fold(
  over set: BalancedSet(a),
  from initial: b,
  with reducer: fn(b, a) -> b,
) -> b {
  balanced_dict.fold(set.dict, initial, fn(acc, k, _) { reducer(acc, k) })
}

pub fn fold_right(
  over set: BalancedSet(a),
  from initial: b,
  with reducer: fn(b, a) -> b,
) -> b {
  balanced_dict.fold_right(set.dict, initial, fn(acc, k, _) { reducer(acc, k) })
}

pub fn from_list(members: List(a), compare: fn(a, a) -> Order) -> BalancedSet(a) {
  let dict =
    list.fold(members, balanced_dict.new(compare), fn(acc, key) {
      balanced_dict.insert(acc, key, Nil)
    })
  BalancedSet(dict, compare)
}

pub fn get_min(from set: BalancedSet(a)) -> Result(a, Nil) {
  set.dict |> balanced_dict.get_min() |> result.map(fn(kvp) { kvp.0 })
}

pub fn get_max(from set: BalancedSet(a)) -> Result(a, Nil) {
  set.dict |> balanced_dict.get_max() |> result.map(fn(kvp) { kvp.0 })
}

pub fn insert(into set: BalancedSet(a), this member: a) -> BalancedSet(a) {
  BalancedSet(..set, dict: balanced_dict.insert(set.dict, member, Nil))
}

pub fn intersection(
  of first: BalancedSet(a),
  and second: BalancedSet(a),
) -> BalancedSet(a) {
  let #(larger, smaller) = order(first, second)
  take(from: larger, keeping: to_asc_list(smaller))
}

pub fn is_disjoint(first: BalancedSet(a), from second: BalancedSet(a)) -> Bool {
  is_empty(intersection(of: first, and: second))
}

pub fn is_empty(set: BalancedSet(a)) -> Bool {
  balanced_dict.size(set.dict) == 0
}

pub fn is_subset(first: BalancedSet(a), of second: BalancedSet(a)) -> Bool {
  intersection(of: first, and: second) |> to_asc_list()
  == first |> to_asc_list()
}

pub fn map(
  from set: BalancedSet(a),
  using compare: fn(b, b) -> Order,
  with fun: fn(a) -> b,
) -> BalancedSet(b) {
  let BalancedSet(dict, _) = set
  let new_dict =
    balanced_dict.fold(dict, balanced_dict.new(compare), fn(acc, key, _) {
      balanced_dict.insert(acc, fun(key), Nil)
    })
  BalancedSet(new_dict, compare)
}

pub fn new(compare: fn(v, v) -> Order) -> BalancedSet(v) {
  BalancedSet(balanced_dict.new(compare), compare)
}

pub fn reorder(
  this set: BalancedSet(a),
  with compare: fn(a, a) -> Order,
) -> BalancedSet(a) {
  fold(set, new(compare), insert)
}

pub fn size(set: BalancedSet(v)) -> Int {
  balanced_dict.size(set.dict)
}

pub fn symmetric_difference(
  of first: BalancedSet(a),
  and second: BalancedSet(a),
) -> BalancedSet(a) {
  difference(
    from: union(of: first, and: second),
    minus: intersection(of: first, and: second),
  )
}

pub fn take(
  from set: BalancedSet(a),
  keeping desired: List(a),
) -> BalancedSet(a) {
  BalancedSet(..set, dict: balanced_dict.take(set.dict, desired))
}

pub fn to_asc_list(set: BalancedSet(a)) -> List(a) {
  set.dict |> balanced_dict.to_asc_list() |> list.map(fn(kvp) { kvp.0 })
}

pub fn to_desc_list(set: BalancedSet(a)) -> List(a) {
  set.dict |> balanced_dict.to_desc_list() |> list.map(fn(kvp) { kvp.0 })
}

pub fn union(
  of first: BalancedSet(a),
  and second: BalancedSet(a),
) -> BalancedSet(a) {
  let #(larger, smaller) = order(first, second)
  fold(smaller, larger, fn(acc, v) { insert(acc, v) })
}

pub fn view_min(from set: BalancedSet(a)) -> Result(#(a, BalancedSet(a)), Nil) {
  set.dict
  |> balanced_dict.view_min()
  |> result.map(fn(r) {
    let #(kvp, new_dict) = r
    #(kvp.0, BalancedSet(..set, dict: new_dict))
  })
}

pub fn view_max(from set: BalancedSet(a)) -> Result(#(a, BalancedSet(a)), Nil) {
  set.dict
  |> balanced_dict.view_max()
  |> result.map(fn(r) {
    let #(kvp, new_dict) = r
    #(kvp.0, BalancedSet(..set, dict: new_dict))
  })
}

fn order(
  first: BalancedSet(a),
  second: BalancedSet(a),
) -> #(BalancedSet(a), BalancedSet(a)) {
  case size(first) > size(second) {
    True -> #(first, second)
    False -> #(second, first)
  }
}
