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
) -> KVBinaryTree(k, v) {
  case root {
    Tip -> Tip
    Branch(_, other_key, other_value, left, right) ->
      case compare(key, other_key) {
        Lt ->
          balance_right(
            other_key,
            other_value,
            delete(left, compare, key),
            right,
          )
        Gt ->
          balance_left(other_key, other_value, left, delete(left, compare, key))
        Eq -> glue(left, right)
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

pub fn tree_size(tree: KVBinaryTree(k, v)) -> Int {
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
          #(orig_value, balance_left(other_key, value, branch, right))
        }
        Gt -> {
          let #(orig_value, branch) = update(right, compare, key, fun)
          #(orig_value, balance_right(other_key, value, left, branch))
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
      MinView(mv_key, mv_value, balance_left(key, value, mv_left, right))
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
      MaxView(mv_key, mv_value, balance_right(key, value, left, mv_right))
    }
  }
}

fn balance_left(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> KVBinaryTree(k, v) {
  case left, right {
    Branch(l_size, _, _, _, _), Branch(r_size, _, _, _, _)
      if l_size <= delta * r_size
    -> Branch(1 + l_size + r_size, key, value, left, right)
    _, _ ->
      case right {
        Tip ->
          case left {
            Tip -> Branch(1, key, value, Tip, Tip)
            Branch(l_size, l_key, l_value, l_left, l_right) -> {
              case l_left, l_right {
                Tip, Tip -> Branch(2, key, value, left, Tip)
                Tip, Branch(_, lr_key, lr_value, _, _) -> {
                  let new_left = Branch(1, l_key, l_value, Tip, Tip)
                  let new_right = Branch(1, key, value, Tip, Tip)
                  Branch(3, lr_key, lr_value, new_left, new_right)
                }
                Branch(_, _, _, _, _), Tip -> {
                  let new_right = Branch(1, key, value, Tip, Tip)
                  Branch(3, l_key, l_value, l_left, new_right)
                }
                Branch(ll_size, _, _, _, _),
                  Branch(lr_size, lr_key, lr_value, lr_left, lr_right)
                -> {
                  case lr_size < ratio * ll_size {
                    True -> {
                      let new_right =
                        Branch(1 + lr_size, key, value, l_right, Tip)
                      Branch(1 + l_size, l_key, l_value, l_left, new_right)
                    }
                    False -> {
                      let new_left =
                        Branch(
                          1 + ll_size + tree_size(lr_left),
                          l_key,
                          l_value,
                          l_left,
                          lr_left,
                        )
                      let new_right =
                        Branch(
                          1 + tree_size(lr_right),
                          key,
                          value,
                          lr_right,
                          Tip,
                        )
                      Branch(1 + l_size, lr_key, lr_value, new_left, new_right)
                    }
                  }
                }
              }
            }
          }
        Branch(r_size, _, _, _, _) ->
          case left {
            Tip -> Branch(1 + r_size, key, value, Tip, right)
            Branch(l_size, l_key, l_value, l_left, l_right) ->
              case l_left, l_right {
                Branch(ll_size, _, _, _, _),
                  Branch(lr_size, lr_key, lr_value, lr_left, lr_right)
                ->
                  case lr_size < ratio * ll_size {
                    True -> {
                      let new_right =
                        Branch(1 + r_size + lr_size, key, value, l_right, right)
                      Branch(
                        1 + l_size + r_size,
                        l_key,
                        l_value,
                        l_left,
                        new_right,
                      )
                    }
                    False -> {
                      let new_left =
                        Branch(
                          1 + ll_size + tree_size(lr_left),
                          l_key,
                          l_value,
                          l_left,
                          lr_left,
                        )
                      let new_right =
                        Branch(
                          1 + r_size + tree_size(lr_right),
                          key,
                          value,
                          lr_right,
                          right,
                        )
                      Branch(
                        1 + l_size + r_size,
                        lr_key,
                        lr_value,
                        new_left,
                        new_right,
                      )
                    }
                  }
                _, _ -> panic as "Failure in balance_left"
              }
          }
      }
  }
}

fn balance_right(
  key: k,
  value: v,
  left: KVBinaryTree(k, v),
  right: KVBinaryTree(k, v),
) -> KVBinaryTree(k, v) {
  case left, right {
    Branch(l_size, _, _, _, _), Branch(r_size, _, _, _, _)
      if r_size <= delta * l_size
    -> Branch(1 + l_size + r_size, key, value, left, right)
    _, _ -> {
      case left {
        Tip ->
          case right {
            Tip -> Branch(1, key, value, Tip, Tip)
            Branch(r_size, r_key, r_value, r_left, r_right) ->
              case r_left, r_right {
                Tip, Tip -> Branch(2, key, value, Tip, right)
                Tip, Branch(_, _, _, _, _) -> {
                  let new_left = Branch(1, key, value, Tip, Tip)
                  Branch(3, r_key, r_value, new_left, r_right)
                }
                Branch(_, rl_key, rl_value, _, _), Tip -> {
                  let new_left = Branch(1, key, value, Tip, Tip)
                  let new_right = Branch(1, r_key, r_value, Tip, Tip)
                  Branch(3, rl_key, rl_value, new_left, new_right)
                }
                Branch(rl_size, rl_key, rl_value, rl_left, rl_right),
                  Branch(rr_size, _, _, _, _)
                ->
                  case rl_size < ratio * rr_size {
                    True -> {
                      let new_left =
                        Branch(1 + rl_size, key, value, Tip, r_left)
                      Branch(1 + r_size, r_key, r_value, new_left, r_right)
                    }
                    False -> {
                      let new_left =
                        Branch(1 + tree_size(rl_left), key, value, Tip, rl_left)
                      let new_right =
                        Branch(
                          1 + rr_size + tree_size(rl_right),
                          r_key,
                          r_value,
                          rl_right,
                          r_right,
                        )
                      Branch(1 + r_size, rl_key, rl_value, new_left, new_right)
                    }
                  }
              }
          }
        Branch(l_size, _, _, _, _) ->
          case right {
            Tip -> Tip
            Branch(r_size, r_key, r_value, r_left, r_right) ->
              case r_left, r_right {
                Branch(rl_size, rl_key, rl_value, rl_left, rl_right),
                  Branch(rr_size, _, _, _, _)
                ->
                  case rl_size < ratio * rr_size {
                    True -> {
                      let new_left =
                        Branch(1 + l_size + rl_size, key, value, left, r_left)
                      Branch(
                        1 + l_size + r_size,
                        r_key,
                        r_value,
                        new_left,
                        r_right,
                      )
                    }
                    False -> {
                      let new_left =
                        Branch(
                          1 + l_size + tree_size(rl_left),
                          key,
                          value,
                          left,
                          rl_left,
                        )
                      let new_right =
                        Branch(
                          1 + rr_size + tree_size(rl_right),
                          r_key,
                          r_value,
                          rl_right,
                          r_right,
                        )
                      Branch(
                        1 + l_size + r_size,
                        rl_key,
                        rl_value,
                        new_left,
                        new_right,
                      )
                    }
                  }
                _, _ -> panic as "Failure in balance_right"
              }
          }
      }
    }
  }
}
