import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}

const delta: Int = 3

const ratio: Int = 2

/// Internal
/// Associated insertion / deletion functions, et al, require a compare function
/// that in encapsulated by `BalancedTree`
pub opaque type KVBinaryTree(a, b) {
  Branch(
    size: Int,
    key: a,
    value: b,
    left: KVBinaryTree(a, b),
    right: KVBinaryTree(a, b),
  )
  Tip
}

// Tip constructor
pub fn tip() -> KVBinaryTree(a, b) {
  Tip
}

pub fn singleton(key: a, value: b) -> KVBinaryTree(a, b) {
  Branch(1, key, value, Tip, Tip)
}

pub fn upsert(
  compare: fn(a, a) -> Order,
  into root: KVBinaryTree(a, b),
  for key: a,
  with fun: fn(Option(b)) -> b,
) -> KVBinaryTree(a, b) {
  case root {
    Tip -> Branch(1, key, fun(None), Tip, Tip)
    Branch(size, other_key, other_value, left, right) ->
      case compare(key, other_key) {
        Lt ->
          balance_left(key, other_value, upsert(compare, left, key, fun), right)
        Gt ->
          balance_right(
            key,
            other_value,
            left,
            upsert(compare, right, key, fun),
          )
        Eq -> Branch(size, key, fun(Some(other_value)), left, right)
      }
  }
}

pub fn insert(
  compare: fn(a, a) -> Order,
  root: KVBinaryTree(a, b),
  key: a,
  value: b,
) -> KVBinaryTree(a, b) {
  upsert(compare, root, key, fn(_) { value })
}

pub fn delete(
  compare: fn(a, a) -> Order,
  root: KVBinaryTree(a, b),
  key: a,
) -> KVBinaryTree(a, b) {
  case root {
    Tip -> Tip
    Branch(_, other_key, other_value, left, right) ->
      case compare(key, other_key) {
        Lt ->
          balance_right(
            other_key,
            other_value,
            delete(compare, left, key),
            right,
          )
        Gt ->
          balance_left(
            other_key,
            other_value,
            left,
            delete(compare, right, key),
          )
        Eq -> glue(left, right)
      }
  }
}

fn glue(
  left: KVBinaryTree(a, b),
  right: KVBinaryTree(a, b),
) -> KVBinaryTree(a, b) {
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

type MinView(a, b) {
  MinView(key: a, value: b, right_tree: KVBinaryTree(a, b))
}

fn min_view_sure(
  key: a,
  value: b,
  left: KVBinaryTree(a, b),
  right: KVBinaryTree(a, b),
) -> MinView(a, b) {
  case left {
    Tip -> MinView(key, value, right)
    Branch(_, l_key, l_value, l_left, l_right) -> {
      let MinView(mv_key, mv_value, mv_left) =
        min_view_sure(l_key, l_value, l_left, l_right)
      MinView(mv_key, mv_value, balance_left(key, value, mv_left, right))
    }
  }
}

type MaxView(a, b) {
  MaxView(key: a, value: b, left_tree: KVBinaryTree(a, b))
}

fn max_view_sure(
  key: a,
  value: b,
  left: KVBinaryTree(a, b),
  right: KVBinaryTree(a, b),
) -> MaxView(a, b) {
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
  key: a,
  value: b,
  left: KVBinaryTree(a, b),
  right: KVBinaryTree(a, b),
) -> KVBinaryTree(a, b) {
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
  key: a,
  value: b,
  left: KVBinaryTree(a, b),
  right: KVBinaryTree(a, b),
) -> KVBinaryTree(a, b) {
  case left, right {
    Branch(l_size, _, _, _, _), Branch(r_size, _, _, _, _)
      if r_size <= delta * l_size
    -> Branch(1 + l_size + r_size, key, value, left, right)
    _, _ -> {
      case left {
        Tip ->
          case right {
            Tip -> Tip
            Branch(r_size, r_key, r_value, r_left, r_right) ->
              case r_left, r_right {
                Tip, Tip -> Branch(1, key, value, Tip, Tip)
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

pub fn tree_size(tree: KVBinaryTree(a, b)) -> Int {
  case tree {
    Tip -> 0
    Branch(size, _, _, _, _) -> size
  }
}
