import data_structures/internal/kv_binary_tree.{type KVBinaryTree} as tree
import gleam/option.{type Option}
import gleam/order.{type Order}

// combine
// drop
// each
// filter
// fold
// from_list
// get
// has_key
// is_empty
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
  BalancedDict(tree.delete(compare, root, key), compare)
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

pub fn insert(
  into dict: BalancedDict(k, v),
  for key: k,
  insert value: v,
) -> BalancedDict(k, v) {
  let BalancedDict(root, compare) = dict
  BalancedDict(tree.insert(compare, root, key, value), compare)
}

pub fn upsert(
  into dict: BalancedDict(k, v),
  update key: k,
  with fun: fn(Option(v)) -> v,
) -> BalancedDict(k, v) {
  let BalancedDict(root, compare) = dict
  BalancedDict(tree.upsert(compare, root, key, fun), compare)
}

pub fn new(compare: fn(k, k) -> Order) -> BalancedDict(k, v) {
  BalancedDict(tree.tip(), compare)
}

pub fn size(tree: BalancedDict(k, v)) -> Int {
  let BalancedDict(root, _) = tree
  tree.tree_size(root)
}
