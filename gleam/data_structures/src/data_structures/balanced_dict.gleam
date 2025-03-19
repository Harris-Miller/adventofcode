import data_structures/internal/kv_binary_tree.{type KVBinaryTree} as tree
import gleam/list
import gleam/option.{type Option}
import gleam/order.{type Order}
import gleam/result
import gleam/set

// combine
// each
// filter

// from_list
// keys
// map_values
// merge

// to_list

// values

pub opaque type BalancedDict(k, v) {
  BalancedDict(root: KVBinaryTree(k, v), compare: fn(k, k) -> Order)
}

pub fn clear(dict: BalancedDict(k, v)) {
  let BalancedDict(_, compare) = dict
  new(compare)
}

pub fn delete(
  from dict: BalancedDict(k, v),
  delete key: k,
) -> BalancedDict(k, v) {
  get_and_delete(dict, key).1
}

pub fn drop(
  from dict: BalancedDict(k, v),
  drop disallowed_keys: List(k),
) -> BalancedDict(k, v) {
  let BalancedDict(root, compare) = dict
  let keys_set = set.from_list(disallowed_keys)
  let new_root = tree.filter(root, fn(k, _) { !set.contains(keys_set, k) })
  BalancedDict(new_root, compare)
}

pub fn filter(
  in dict: BalancedDict(k, v),
  keeping predicate: fn(k, v) -> Bool,
) -> BalancedDict(k, v) {
  let BalancedDict(root, compare) = dict
  let new_root = tree.filter(root, predicate)
  BalancedDict(new_root, compare)
}

pub fn fold(
  over dict: BalancedDict(k, v),
  from initial: acc,
  with fun: fn(acc, k, v) -> acc,
) -> acc {
  let BalancedDict(root, _) = dict
  tree.fold(root, initial, fun)
}

pub fn fold_right(
  over dict: BalancedDict(k, v),
  from initial: acc,
  with fun: fn(acc, k, v) -> acc,
) -> acc {
  let BalancedDict(root, _) = dict
  tree.fold_right(root, initial, fun)
}

pub fn from_list(
  list: List(#(k, v)),
  compare: fn(k, k) -> Order,
) -> BalancedDict(k, v) {
  list.fold(list, new(compare), fn(dict, kvp) {
    let #(key, value) = kvp
    insert(dict, key, value)
  })
}

pub fn get(from dict: BalancedDict(k, v), get key: k) -> Result(v, Nil) {
  let BalancedDict(root, compare) = dict
  tree.get(root, compare, key)
}

pub fn get_and_delete(
  from dict: BalancedDict(k, v),
  delete key: k,
) -> #(Option(v), BalancedDict(k, v)) {
  let BalancedDict(root, compare) = dict
  let #(value, new_root) = tree.delete(root, compare, key)
  #(value, BalancedDict(new_root, compare))
}

pub fn get_and_insert(
  into dict: BalancedDict(k, v),
  update key: k,
  insert value: v,
) -> #(Option(v), BalancedDict(k, v)) {
  get_and_upsert(dict, key, fn(_) { value })
}

pub fn get_and_upsert(
  into dict: BalancedDict(k, v),
  update key: k,
  with fun: fn(Option(v)) -> v,
) -> #(Option(v), BalancedDict(k, v)) {
  let BalancedDict(root, compare) = dict
  let #(old_value, new_root) = tree.update(root, compare, key, fun)
  #(old_value, BalancedDict(new_root, compare))
}

pub fn get_min(from dict: BalancedDict(k, v)) -> Result(#(k, v), Nil) {
  let BalancedDict(root, _) = dict
  tree.get_min(root)
}

pub fn get_max(from dict: BalancedDict(k, v)) -> Result(#(k, v), Nil) {
  let BalancedDict(root, _) = dict
  tree.get_max(root)
}

pub fn has_key(from dict: BalancedDict(k, v), get key: k) -> Bool {
  case get(dict, key) {
    Ok(_) -> True
    Error(_) -> False
  }
}

pub fn insert(
  into dict: BalancedDict(k, v),
  for key: k,
  insert value: v,
) -> BalancedDict(k, v) {
  get_and_insert(dict, key, value).1
}

pub fn is_empty(dict: BalancedDict(k, v)) -> Bool {
  size(dict) == 0
}

pub fn map_values(
  in dict: BalancedDict(k, v1),
  with fun: fn(k, v1) -> v2,
) -> BalancedDict(k, v2) {
  let BalancedDict(root, compare) = dict
  let new_root = tree.map(root, fun)
  BalancedDict(new_root, compare)
}

pub fn new(compare: fn(k, k) -> Order) -> BalancedDict(k, v) {
  BalancedDict(tree.tip(), compare)
}

pub fn size(dict: BalancedDict(k, v)) -> Int {
  let BalancedDict(root, _) = dict
  tree.size(root)
}

pub fn take(
  from dict: BalancedDict(k, v),
  keeping desired_keys: List(k),
) -> BalancedDict(k, v) {
  case desired_keys {
    [] -> clear(dict)
    _ -> {
      let BalancedDict(root, compare) = dict
      let keys_set = set.from_list(desired_keys)
      let new_root = tree.filter(root, fn(k, _) { set.contains(keys_set, k) })
      BalancedDict(new_root, compare)
    }
  }
}

pub fn to_asc_list(dict: BalancedDict(k, v)) -> List(#(k, v)) {
  let BalancedDict(root, _) = dict
  tree.to_asc_list(root)
}

pub fn to_desc_list(dict: BalancedDict(k, v)) -> List(#(k, v)) {
  let BalancedDict(root, _) = dict
  tree.to_desc_list(root)
}

pub fn upsert(
  into dict: BalancedDict(k, v),
  update key: k,
  with fun: fn(Option(v)) -> v,
) -> BalancedDict(k, v) {
  get_and_upsert(dict, key, fun).1
}

pub fn view_min(
  from dict: BalancedDict(k, v),
) -> Result(#(#(k, v), BalancedDict(k, v)), Nil) {
  let BalancedDict(root, compare) = dict
  tree.view_min(root)
  |> result.map(fn(res) {
    let #(kvp, new_root) = res
    #(kvp, BalancedDict(new_root, compare))
  })
}

pub fn view_max(
  from dict: BalancedDict(k, v),
) -> Result(#(#(k, v), BalancedDict(k, v)), Nil) {
  let BalancedDict(root, compare) = dict
  tree.view_max(root)
  |> result.map(fn(res) {
    let #(kvp, new_root) = res
    #(kvp, BalancedDict(new_root, compare))
  })
}
