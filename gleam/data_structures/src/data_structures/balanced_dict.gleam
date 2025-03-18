import data_structures/internal/kv_binary_tree.{type KVBinaryTree} as tree
import gleam/option.{type Option}
import gleam/order.{type Order}

// combine
// delete
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

pub opaque type BalancedDict(a, b) {
  BalancedDict(root: KVBinaryTree(a, b), compare: fn(a, a) -> Order)
}

pub fn insert(
  into dict: BalancedDict(a, b),
  for key: a,
  insert value: b,
) -> BalancedDict(a, b) {
  let BalancedDict(root, compare) = dict
  BalancedDict(tree.insert(compare, root, key, value), compare)
}

pub fn upsert(
  into dict: BalancedDict(a, b),
  update key: a,
  with fun: fn(Option(b)) -> b,
) -> BalancedDict(a, b) {
  let BalancedDict(root, compare) = dict
  BalancedDict(tree.upsert(compare, root, key, fun), compare)
}

pub fn new(compare: fn(a, a) -> Order) -> BalancedDict(a, b) {
  BalancedDict(tree.tip(), compare)
}

pub fn size(tree: BalancedDict(a, b)) -> Int {
  let BalancedDict(root, _) = tree
  tree.tree_size(root)
}
