import data_structures/internal/kv_binary_tree.{type KVBinaryTree} as tree
import gleam/list
import gleam/option.{type Option}
import gleam/order.{type Order}

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

pub fn delete(
  from dict: BalancedDict(k, v),
  delete key: k,
) -> BalancedDict(k, v) {
  let BalancedDict(root, compare) = dict
  BalancedDict(tree.delete(root, compare, key), compare)
}

pub fn drop(
  from dict: BalancedDict(k, v),
  drop disallowed_keys: List(k),
) -> BalancedDict(k, v) {
  case disallowed_keys {
    [] -> dict
    [first, ..rest] -> drop(delete(dict, first), rest)
  }
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

pub fn get_and_insert(
  into dict: BalancedDict(k, v),
  update key: k,
  insert value: v,
) -> #(Option(v), BalancedDict(k, v)) {
  let BalancedDict(root, compare) = dict
  let #(old_value, new_root) = tree.update(root, compare, key, fn(_) { value })
  #(old_value, BalancedDict(new_root, compare))
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
  tree.tree_size(root)
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
