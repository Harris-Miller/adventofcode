import data_structures/internal/kv_binary_tree.{type KVBinaryTree} as tree
import gleam/list
import gleam/option.{type Option}
import gleam/order.{type Order}
import gleam/pair
import gleam/result
import gleam/set

pub opaque type BalancedMap(k, v) {
  BalancedMap(root: KVBinaryTree(k, v), compare: fn(k, k) -> Order)
}

pub fn clear(map: BalancedMap(k, v)) -> BalancedMap(k, v) {
  let BalancedMap(_, compare) = map
  new(compare)
}

pub fn combine(
  map: BalancedMap(k, v),
  other: BalancedMap(k, v),
  with fun: fn(v, v) -> v,
) -> BalancedMap(k, v) {
  fold(map, other, fn(acc, key, value) {
    case get(acc, key) {
      Ok(other_value) -> insert(acc, key, fun(value, other_value))
      Error(_) -> insert(acc, key, value)
    }
  })
}

pub fn delete(from map: BalancedMap(k, v), delete key: k) -> BalancedMap(k, v) {
  get_and_delete(map, key).1
}

pub fn drop(
  from map: BalancedMap(k, v),
  drop disallowed_keys: List(k),
) -> BalancedMap(k, v) {
  let keys_set = set.from_list(disallowed_keys)
  let new_root = tree.filter(map.root, fn(k, _) { !set.contains(keys_set, k) })
  BalancedMap(..map, root: new_root)
}

pub fn each(map: BalancedMap(k, v), fun: fn(k, v) -> z) -> Nil {
  fold(map, Nil, fn(nil, k, v) {
    fun(k, v)
    nil
  })
}

pub fn filter(
  in map: BalancedMap(k, v),
  keeping predicate: fn(k, v) -> Bool,
) -> BalancedMap(k, v) {
  BalancedMap(..map, root: tree.filter(map.root, predicate))
}

pub fn fold(
  over map: BalancedMap(k, v),
  from initial: acc,
  with fun: fn(acc, k, v) -> acc,
) -> acc {
  tree.fold(map.root, initial, fun)
}

pub fn fold_right(
  over map: BalancedMap(k, v),
  from initial: acc,
  with fun: fn(acc, k, v) -> acc,
) -> acc {
  tree.fold_right(map.root, initial, fun)
}

pub fn from_list(
  list: List(#(k, v)),
  compare: fn(k, k) -> Order,
) -> BalancedMap(k, v) {
  list.fold(list, new(compare), fn(map, kvp) {
    let #(key, value) = kvp
    insert(map, key, value)
  })
}

pub fn get(from map: BalancedMap(k, v), get key: k) -> Result(v, Nil) {
  let BalancedMap(root, compare) = map
  tree.get(root, compare, key)
}

pub fn get_and_delete(
  from map: BalancedMap(k, v),
  delete key: k,
) -> #(Option(v), BalancedMap(k, v)) {
  let BalancedMap(root, compare) = map
  let #(value, new_root) = tree.delete(root, compare, key)
  #(value, BalancedMap(new_root, compare))
}

pub fn get_and_insert(
  into map: BalancedMap(k, v),
  update key: k,
  insert value: v,
) -> #(Option(v), BalancedMap(k, v)) {
  get_and_upsert(map, key, fn(_) { value })
}

pub fn get_and_upsert(
  into map: BalancedMap(k, v),
  update key: k,
  with fun: fn(Option(v)) -> v,
) -> #(Option(v), BalancedMap(k, v)) {
  let BalancedMap(root, compare) = map
  let #(old_value, new_root) = tree.update(root, compare, key, fun)
  #(old_value, BalancedMap(new_root, compare))
}

pub fn get_min(from map: BalancedMap(k, v)) -> Result(#(k, v), Nil) {
  tree.get_min(map.root)
}

pub fn get_max(from map: BalancedMap(k, v)) -> Result(#(k, v), Nil) {
  tree.get_max(map.root)
}

pub fn has_key(from map: BalancedMap(k, v), get key: k) -> Bool {
  case get(map, key) {
    Ok(_) -> True
    Error(_) -> False
  }
}

pub fn insert(
  into map: BalancedMap(k, v),
  for key: k,
  insert value: v,
) -> BalancedMap(k, v) {
  get_and_insert(map, key, value).1
}

pub fn is_empty(map: BalancedMap(k, v)) -> Bool {
  size(map) == 0
}

pub fn keys(map: BalancedMap(k, v)) -> List(k) {
  map |> to_asc_list |> list.map(pair.first)
}

pub fn map_values(
  in map: BalancedMap(k, v1),
  with fun: fn(k, v1) -> v2,
) -> BalancedMap(k, v2) {
  BalancedMap(..map, root: tree.map(map.root, fun))
}

pub fn merge(
  into map: BalancedMap(k, v),
  from new_entries: BalancedMap(k, v),
) -> BalancedMap(k, v) {
  fold(new_entries, map, fn(acc, key, value) { insert(acc, key, value) })
}

pub fn new(compare: fn(k, k) -> Order) -> BalancedMap(k, v) {
  BalancedMap(tree.tip(), compare)
}

pub fn reorder(
  this map: BalancedMap(k, v),
  with compare: fn(k, k) -> Order,
) -> BalancedMap(k, v) {
  fold(map, new(compare), insert)
}

pub fn size(map: BalancedMap(k, v)) -> Int {
  tree.size(map.root)
}

pub fn take(
  from map: BalancedMap(k, v),
  keeping desired_keys: List(k),
) -> BalancedMap(k, v) {
  case desired_keys {
    [] -> clear(map)
    _ -> {
      let keys_set = set.from_list(desired_keys)
      let new_root =
        tree.filter(map.root, fn(k, _) { set.contains(keys_set, k) })
      BalancedMap(..map, root: new_root)
    }
  }
}

pub fn to_asc_list(map: BalancedMap(k, v)) -> List(#(k, v)) {
  tree.to_asc_list(map.root)
}

pub fn to_desc_list(map: BalancedMap(k, v)) -> List(#(k, v)) {
  tree.to_desc_list(map.root)
}

pub fn upsert(
  into map: BalancedMap(k, v),
  update key: k,
  with fun: fn(Option(v)) -> v,
) -> BalancedMap(k, v) {
  get_and_upsert(map, key, fun).1
}

pub fn values(map: BalancedMap(k, v)) -> List(v) {
  map |> to_asc_list |> list.map(pair.second)
}

pub fn view_min(
  from map: BalancedMap(k, v),
) -> Result(#(#(k, v), BalancedMap(k, v)), Nil) {
  tree.view_min(map.root)
  |> result.map(fn(res) {
    let #(kvp, new_root) = res
    #(kvp, BalancedMap(..map, root: new_root))
  })
}

pub fn view_max(
  from map: BalancedMap(k, v),
) -> Result(#(#(k, v), BalancedMap(k, v)), Nil) {
  tree.view_max(map.root)
  |> result.map(fn(res) {
    let #(kvp, new_root) = res
    #(kvp, BalancedMap(..map, root: new_root))
  })
}
