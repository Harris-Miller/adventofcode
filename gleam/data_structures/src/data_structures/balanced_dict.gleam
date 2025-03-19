import data_structures/internal/kv_binary_tree.{type KVBinaryTree} as tree
import gleam/list
import gleam/option.{type Option}
import gleam/order.{type Order}
import gleam/pair
import gleam/result
import gleam/set

pub opaque type BalancedDict(k, v) {
  BalancedDict(root: KVBinaryTree(k, v), compare: fn(k, k) -> Order)
}

pub fn clear(dict: BalancedDict(k, v)) -> BalancedDict(k, v) {
  let BalancedDict(_, compare) = dict
  new(compare)
}

pub fn combine(
  dict: BalancedDict(k, v),
  other: BalancedDict(k, v),
  with fun: fn(v, v) -> v,
) -> BalancedDict(k, v) {
  fold(dict, other, fn(acc, key, value) {
    case get(acc, key) {
      Ok(other_value) -> insert(acc, key, fun(value, other_value))
      Error(_) -> insert(acc, key, value)
    }
  })
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
  let keys_set = set.from_list(disallowed_keys)
  let new_root = tree.filter(dict.root, fn(k, _) { !set.contains(keys_set, k) })
  BalancedDict(..dict, root: new_root)
}

pub fn each(dict: BalancedDict(k, v), fun: fn(k, v) -> z) -> Nil {
  fold(dict, Nil, fn(nil, k, v) {
    fun(k, v)
    nil
  })
}

pub fn filter(
  in dict: BalancedDict(k, v),
  keeping predicate: fn(k, v) -> Bool,
) -> BalancedDict(k, v) {
  BalancedDict(..dict, root: tree.filter(dict.root, predicate))
}

pub fn fold(
  over dict: BalancedDict(k, v),
  from initial: acc,
  with fun: fn(acc, k, v) -> acc,
) -> acc {
  tree.fold(dict.root, initial, fun)
}

pub fn fold_right(
  over dict: BalancedDict(k, v),
  from initial: acc,
  with fun: fn(acc, k, v) -> acc,
) -> acc {
  tree.fold_right(dict.root, initial, fun)
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
  tree.get_min(dict.root)
}

pub fn get_max(from dict: BalancedDict(k, v)) -> Result(#(k, v), Nil) {
  tree.get_max(dict.root)
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

pub fn keys(dict: BalancedDict(k, v)) -> List(k) {
  dict |> to_asc_list |> list.map(pair.first)
}

pub fn map_values(
  in dict: BalancedDict(k, v1),
  with fun: fn(k, v1) -> v2,
) -> BalancedDict(k, v2) {
  BalancedDict(..dict, root: tree.map(dict.root, fun))
}

pub fn merge(
  into dict: BalancedDict(k, v),
  from new_entries: BalancedDict(k, v),
) -> BalancedDict(k, v) {
  fold(new_entries, dict, fn(acc, key, value) { insert(acc, key, value) })
}

pub fn new(compare: fn(k, k) -> Order) -> BalancedDict(k, v) {
  BalancedDict(tree.tip(), compare)
}

pub fn reorder(
  this dict: BalancedDict(k, v),
  with compare: fn(k, k) -> Order,
) -> BalancedDict(k, v) {
  fold(dict, new(compare), insert)
}

pub fn size(dict: BalancedDict(k, v)) -> Int {
  tree.size(dict.root)
}

pub fn take(
  from dict: BalancedDict(k, v),
  keeping desired_keys: List(k),
) -> BalancedDict(k, v) {
  case desired_keys {
    [] -> clear(dict)
    _ -> {
      let keys_set = set.from_list(desired_keys)
      let new_root =
        tree.filter(dict.root, fn(k, _) { set.contains(keys_set, k) })
      BalancedDict(..dict, root: new_root)
    }
  }
}

pub fn to_asc_list(dict: BalancedDict(k, v)) -> List(#(k, v)) {
  tree.to_asc_list(dict.root)
}

pub fn to_desc_list(dict: BalancedDict(k, v)) -> List(#(k, v)) {
  tree.to_desc_list(dict.root)
}

pub fn upsert(
  into dict: BalancedDict(k, v),
  update key: k,
  with fun: fn(Option(v)) -> v,
) -> BalancedDict(k, v) {
  get_and_upsert(dict, key, fun).1
}

pub fn values(dict: BalancedDict(k, v)) -> List(v) {
  dict |> to_asc_list |> list.map(pair.second)
}

pub fn view_min(
  from dict: BalancedDict(k, v),
) -> Result(#(#(k, v), BalancedDict(k, v)), Nil) {
  tree.view_min(dict.root)
  |> result.map(fn(res) {
    let #(kvp, new_root) = res
    #(kvp, BalancedDict(..dict, root: new_root))
  })
}

pub fn view_max(
  from dict: BalancedDict(k, v),
) -> Result(#(#(k, v), BalancedDict(k, v)), Nil) {
  tree.view_max(dict.root)
  |> result.map(fn(res) {
    let #(kvp, new_root) = res
    #(kvp, BalancedDict(..dict, root: new_root))
  })
}
