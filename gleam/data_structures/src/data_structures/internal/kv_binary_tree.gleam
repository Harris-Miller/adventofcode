import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}

const delta: Int = 3

const ratio: Int = 2

/// Internal
/// Associated insertion / deletion functions, et al, require a compare function
/// that in encapsulated by `BalancedTree`
pub opaque type KVBinaryTree(k, v) {
  Branch(
    size: Int,
    key: k,
    value: v,
    left: KVBinaryTree(k, v),
    right: KVBinaryTree(k, v),
  )
  Tip
}

pub fn delete(
  from root: KVBinaryTree(k, v),
  with compare: fn(k, k) -> Order,
  delete key: k,
) -> #(Option(v), KVBinaryTree(k, v)) {
  case root {
    Tip -> #(None, Tip)
    Branch(_, other_key, other_value, left, right) ->
      case compare(key, other_key) {
        Lt -> {
          let #(orig_value, branch) = delete(left, compare, key)
          let tree = balance(other_key, other_value, branch, right)
          #(orig_value, tree)
        }
        Gt -> {
          let #(orig_value, branch) = delete(right, compare, key)
          let tree = balance(other_key, other_value, left, branch)
          #(orig_value, tree)
        }
        Eq -> #(Some(other_value), glue(left, right))
      }
  }
}

pub fn filter(
  root: KVBinaryTree(k, v),
  predicate: fn(k, v) -> Bool,
) -> KVBinaryTree(k, v) {
  case root {
    Tip -> Tip
    Branch(_, key, value, left, right) -> {
      let pl = filter(left, predicate)
      let pr = filter(right, predicate)

      case predicate(key, value), pl, pr {
        True, Branch(_, l_key, _, _, _), Branch(_, r_key, _, _, _)
          if key == l_key && key == r_key
        -> root
        True, _, _ -> link(key, value, pl, pr)
        _, _, _ -> merge_trees(pl, pr)
      }
    }
  }
}

pub fn fold(
  over root: KVBinaryTree(k, v),
  from initial: acc,
  with fun: fn(acc, k, v) -> acc,
) -> acc {
  case root {
    Tip -> initial
    Branch(_, key, value, left, right) ->
      fold(right, fun(fold(left, initial, fun), key, value), fun)
  }
}

pub fn fold_right(
  over root: KVBinaryTree(k, v),
  from initial: acc,
  with fun: fn(acc, k, v) -> acc,
) -> acc {
  case root {
    Tip -> initial
    Branch(_, key, value, left, right) ->
      fold_right(left, fun(fold_right(right, initial, fun), key, value), fun)
  }
}

pub fn get(
  from root: KVBinaryTree(k, v),
  with compare: fn(k, k) -> Order,
  get key: k,
) -> Result(v, Nil) {
  case root {
    Tip -> Error(Nil)
    Branch(_, branch_key, branch_value, left, right) ->
      case compare(key, branch_key) {
        Lt -> get(left, compare, key)
        Gt -> get(right, compare, key)
        Eq -> Ok(branch_value)
      }
  }
}

pub fn map(
  in root: KVBinaryTree(k, v1),
  with fun: fn(k, v1) -> v2,
) -> KVBinaryTree(k, v2) {
  case root {
    Tip -> Tip
    Branch(size, key, value, left, right) ->
      Branch(size, key, fun(key, value), map(left, fun), map(right, fun))
  }
}

pub fn to_asc_list(root: KVBinaryTree(k, v)) -> List(#(k, v)) {
  fold_right(root, [], fn(acc, k, v) { [#(k, v), ..acc] })
}

pub fn to_desc_list(root: KVBinaryTree(k, v)) -> List(#(k, v)) {
  fold(root, [], fn(acc, k, v) { [#(k, v), ..acc] })
}

pub fn size(tree: KVBinaryTree(k, v)) -> Int {
  case tree {
    Tip -> 0
    Branch(size, _, _, _, _) -> size
  }
}

pub fn update(
  into root: KVBinaryTree(k, v),
  with compare: fn(k, k) -> Order,
  for key: k,
  using fun: fn(Option(v)) -> v,
) -> #(Option(v), KVBinaryTree(k, v)) {
  case root {
    Tip -> #(None, Branch(1, key, fun(None), Tip, Tip))
    Branch(size, other_key, value, left, right) ->
      case compare(key, other_key) {
        Lt -> {
          let #(orig_value, branch) = update(left, compare, key, fun)
          let tree = balance(other_key, value, branch, right)
          #(orig_value, tree)
        }
        Gt -> {
          let #(orig_value, branch) = update(right, compare, key, fun)
          let tree = balance(other_key, value, left, branch)
          #(orig_value, tree)
        }
        Eq -> #(Some(value), Branch(size, key, fun(Some(value)), left, right))
      }
  }
}

pub fn view_min(
  root: KVBinaryTree(k, v),
) -> Result(#(#(k, v), KVBinaryTree(k, v)), Nil) {
  case root {
    Tip -> Error(Nil)
    Branch(_, key, value, left, right) -> {
      let #(k, v, tree) = remove_min(key, value, left, right)
      Ok(#(#(k, v), tree))
    }
  }
}

pub fn view_max(
  root: KVBinaryTree(k, v),
) -> Result(#(#(k, v), KVBinaryTree(k, v)), Nil) {
  case root {
    Tip -> Error(Nil)
    Branch(_, key, value, left, right) -> {
      let #(k, v, tree) = remove_max(key, value, left, right)
      Ok(#(#(k, v), tree))
    }
  }
}

fn glue(
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> KVBinaryTree(k, v) {
  case left, right {
    Tip, r -> r
    l, Tip -> l
    Branch(l_size, l_key, l_value, l_left, l_right),
      Branch(r_size, r_key, r_value, r_left, r_right)
    ->
      case l_size > r_size {
        True -> {
          let #(mv_key, mv_value, mv_left) =
            remove_max(l_key, l_value, l_left, l_right)
          Branch(l_size + r_size, mv_key, mv_value, mv_left, right)
        }
        False -> {
          let #(mv_key, mv_value, mv_right) =
            remove_min(r_key, r_value, r_left, r_right)
          Branch(l_size + r_size, mv_key, mv_value, left, mv_right)
        }
      }
  }
}

fn remove_min(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> #(k, v, KVBinaryTree(k, v)) {
  case left {
    Tip -> #(key, value, right)
    Branch(_, l_key, l_value, l_left, l_right) -> {
      let #(mv_key, mv_value, mv_left) =
        remove_min(l_key, l_value, l_left, l_right)
      let tree = balance(key, value, mv_left, right)
      #(mv_key, mv_value, tree)
    }
  }
}

fn remove_max(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> #(k, v, KVBinaryTree(k, v)) {
  case right {
    Tip -> #(key, value, left)
    Branch(_, r_key, r_value, r_left, r_right) -> {
      let #(mv_key, mv_value, mv_right) =
        remove_max(r_key, r_value, r_left, r_right)
      let tree = balance(key, value, left, mv_right)
      #(mv_key, mv_value, tree)
    }
  }
}

fn balance(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> KVBinaryTree(k, v) {
  let size_l = size(left)
  let size_r = size(right)
  let size_x = size_l + size_r + 1

  let a = size_l + size_r <= 1
  let b = size_r > delta * size_l
  let c = size_l > delta * size_r

  case a, b, c {
    True, _, _ -> Branch(size_x, key, value, left, right)
    _, True, _ -> rotate_left(key, value, left, right)
    _, _, True -> rotate_right(key, value, left, right)
    _, _, _ -> Branch(size_x, key, value, left, right)
  }
}

fn rotate_left(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> KVBinaryTree(k, v) {
  let assert Branch(_, _, _, r_left, r_right) = right
  case size(r_left) < ratio * size(r_right) {
    True -> single_left(key, value, left, right)
    False -> double_left(key, value, left, right)
  }
}

fn rotate_right(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> KVBinaryTree(k, v) {
  let assert Branch(_, _, _, l_left, l_right) = left
  case size(l_right) < ratio * size(l_left) {
    True -> single_right(key, value, left, right)
    False -> double_right(key, value, left, right)
  }
}

fn single_left(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> KVBinaryTree(k, v) {
  let assert Branch(_, r_key, r_value, r_left, r_right) = right

  let new_left = make(key, value, left, r_left)

  make(r_key, r_value, new_left, r_right)
}

fn single_right(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> KVBinaryTree(k, v) {
  let assert Branch(_, l_key, l_value, l_left, l_right) = left

  let new_right = make(key, value, l_right, right)

  make(l_key, l_value, l_left, new_right)
}

fn double_left(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> KVBinaryTree(k, v) {
  let assert Branch(_, r_key, r_value, r_left, r_right) = right
  let assert Branch(_, rl_key, rl_value, rl_left, rl_right) = r_left

  let new_left = make(key, value, left, rl_left)
  let new_right = make(r_key, r_value, rl_right, r_right)

  make(rl_key, rl_value, new_left, new_right)
}

fn double_right(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> KVBinaryTree(k, v) {
  let assert Branch(_, l_key, r_key, l_left, l_right) = left
  let assert Branch(_, lr_key, lr_value, lr_left, lr_right) = l_right

  let new_left = make(l_key, r_key, l_left, lr_left)
  let new_right = make(key, value, lr_right, right)

  make(lr_key, lr_value, new_left, new_right)
}

/// constructor maintains the size of the tree
fn make(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> KVBinaryTree(k, v) {
  Branch(size(left) + size(right) + 1, key, value, left, right)
}

pub fn get_min(tree: KVBinaryTree(k, v)) -> Result(#(k, v), Nil) {
  case tree {
    Tip -> Error(Nil)
    _ -> Ok(get_min_loop(tree))
  }
}

fn get_min_loop(tree: KVBinaryTree(k, v)) -> #(k, v) {
  let assert Branch(_, key, value, left, _) = tree
  case left {
    Tip -> #(key, value)
    _ -> get_min_loop(left)
  }
}

pub fn get_max(tree: KVBinaryTree(k, v)) -> Result(#(k, v), Nil) {
  case tree {
    Tip -> Error(Nil)
    _ -> Ok(get_max_loop(tree))
  }
}

pub fn tip() {
  Tip
}

fn get_max_loop(tree: KVBinaryTree(k, v)) -> #(k, v) {
  let assert Branch(_, key, value, _, right) = tree
  case right {
    Tip -> #(key, value)
    _ -> get_max_loop(right)
  }
}

fn link(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> KVBinaryTree(k, v) {
  case left, right {
    Tip, _ -> insert_min(key, value, right)
    _, Tip -> insert_max(key, value, left)
    Branch(l_size, _, _, _, _), Branch(r_size, r_key, r_value, r_left, r_right)
      if delta * l_size < r_size
    -> {
      let new_left = link(key, value, left, r_left)
      balance(r_key, r_value, new_left, r_right)
    }
    Branch(l_size, l_key, l_value, l_left, l_right), Branch(r_size, _, _, _, _)
      if delta * l_size < r_size
    -> {
      let new_right = link(key, value, l_right, right)
      balance(l_key, l_value, l_left, new_right)
    }
    Branch(_, _, _, _, _), Branch(_, _, _, _, _) ->
      make(key, value, left, right)
  }
}

fn insert_min(key: k, value: v, tree: KVBinaryTree(k, v)) -> KVBinaryTree(k, v) {
  case tree {
    Tip -> Branch(1, key, value, Tip, Tip)
    Branch(_, k, v, l, r) -> balance(k, v, insert_min(key, value, l), r)
  }
}

fn insert_max(key: k, value: v, tree: KVBinaryTree(k, v)) -> KVBinaryTree(k, v) {
  case tree {
    Tip -> Branch(1, key, value, Tip, Tip)
    Branch(_, k, v, l, r) -> balance(k, v, l, insert_max(key, value, r))
  }
}

/// merge two trees
fn merge_trees(
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> KVBinaryTree(k, v) {
  case left, right {
    Tip, _ -> right
    _, Tip -> left
    Branch(l_size, _, _, _, _), Branch(r_size, r_key, r_value, r_left, r_right)
      if delta * l_size < r_size
    -> {
      let new_left = merge_trees(left, r_left)
      balance(r_key, r_value, new_left, r_right)
    }
    Branch(l_size, l_key, l_value, l_left, l_right), Branch(r_size, _, _, _, _)
      if delta * r_size < l_size
    -> {
      let new_right = merge_trees(l_right, right)
      balance(l_key, l_value, l_left, new_right)
    }
    Branch(_, _, _, _, _), Branch(_, _, _, _, _) -> glue(left, right)
  }
}
