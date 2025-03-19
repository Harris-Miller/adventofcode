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

// Tip constructor
pub fn tip() -> KVBinaryTree(k, v) {
  Tip
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
          let MaxView(mv_key, mv_value, mv_left) =
            max_view_sure(l_key, l_value, l_left, l_right)
          Branch(l_size + r_size, mv_key, mv_value, mv_left, right)
        }
        False -> {
          let MinView(mv_key, mv_value, mv_right) =
            min_view_sure(r_key, r_value, r_left, r_right)
          Branch(l_size + r_size, mv_key, mv_value, left, mv_right)
        }
      }
  }
}

type MinView(k, v) {
  MinView(key: k, value: v, right_tree: KVBinaryTree(k, v))
}

fn min_view_sure(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> MinView(k, v) {
  case left {
    Tip -> MinView(key, value, right)
    Branch(_, l_key, l_value, l_left, l_right) -> {
      let MinView(mv_key, mv_value, mv_left) =
        min_view_sure(l_key, l_value, l_left, l_right)
      // let tree = balance_left(key, value, mv_left, right)
      let tree = balance(key, value, mv_left, right)
      MinView(mv_key, mv_value, tree)
    }
  }
}

type MaxView(k, v) {
  MaxView(key: k, value: v, left_tree: KVBinaryTree(k, v))
}

fn max_view_sure(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> MaxView(k, v) {
  case right {
    Tip -> MaxView(key, value, left)
    Branch(_, r_key, r_value, r_left, r_right) -> {
      let MaxView(mv_key, mv_value, mv_right) =
        max_view_sure(r_key, r_value, r_left, r_right)
      let tree = balance(key, value, left, mv_right)
      MaxView(mv_key, mv_value, tree)
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
