/// An extension of: https://github.com/schurhammer/gleamy_structures/blob/v1.1.0/src/gleamy/pairing_heap.gleam
import gleam/list
import gleam/order.{type Order, Gt}

type Tree(a) {
  Empty
  Tree(a, List(Tree(a)))
}

pub opaque type Heap(a) {
  Heap(root: Tree(a), compare: fn(a, a) -> Order)
}

/// Creates a new empty heap with the provided comparison function.
pub fn new(compare: fn(a, a) -> Order) -> Heap(a) {
  Heap(Empty, compare)
}

/// Checks if key is in queue
pub fn has(heap: Heap(a), key: a) -> Bool {
  has_loop([heap.root], key)
}

fn has_loop(roots: List(Tree(a)), key: a) -> Bool {
  case roots {
    [] -> False
    [Empty, ..] -> False
    [Tree(a, children), ..xs] -> {
      case a == key {
        True -> True
        False -> has_loop(list.append(xs, children), key)
      }
    }
  }
}

/// Inserts a new item into the heap, preserving the heap property.
/// Time complexity: O(1)
pub fn insert(heap: Heap(a), key: a) -> Heap(a) {
  case has(heap, key) {
    True -> heap
    False ->
      Heap(merge_trees(Tree(key, []), heap.root, heap.compare), heap.compare)
  }
}

/// Returns the minimum element in the heap, if the heap is not empty.
/// Time complexity: O(1)
pub fn find_min(heap: Heap(a)) -> Result(a, Nil) {
  case heap.root {
    Tree(x, _) -> Ok(x)
    Empty -> Error(Nil)
  }
}

/// Removes and returns the minimum element from the heap along with the
/// new heap after deletion, if the heap is not empty.
/// Time complexity: O(log n) amortized
pub fn delete_min(heap: Heap(a)) -> Result(#(a, Heap(a)), Nil) {
  case heap.root {
    Tree(x, xs) -> Ok(#(x, Heap(merge_pairs(xs, heap.compare), heap.compare)))
    Empty -> Error(Nil)
  }
}

/// Merges two heaps into a new heap containing all elements from both heaps,
/// preserving the heap property.
/// The given heaps must have the same comparison function.
/// Time complexity: O(1)
pub fn merge(heap1: Heap(a), heap2: Heap(a)) -> Heap(a) {
  let compare = heap1.compare
  Heap(merge_trees(heap1.root, heap2.root, compare), compare)
}

/// Output to list in order
pub fn to_list(heap: Heap(a)) -> List(a) {
  to_list_loop(heap, [])
}

fn to_list_loop(heap: Heap(a), acc: List(a)) -> List(a) {
  case delete_min(heap) {
    Error(_) -> list.reverse(acc)
    Ok(#(a, heap)) -> to_list_loop(heap, [a, ..acc])
  }
}

pub fn count(heap: Heap(a)) -> Int {
  count_loop([heap.root])
}

fn count_loop(roots: List(Tree(a))) -> Int {
  case roots {
    [] -> 0
    [Empty, ..] -> 0
    [Tree(_, children), ..xs] -> 1 + count_loop(list.append(xs, children))
  }
}

fn merge_trees(x: Tree(a), y: Tree(a), compare: fn(a, a) -> Order) -> Tree(a) {
  case x, y {
    x, Empty -> x
    Empty, y -> y
    Tree(xk, xs), Tree(yk, ys) ->
      case compare(xk, yk) {
        Gt -> Tree(yk, [x, ..ys])
        _ -> Tree(xk, [y, ..xs])
      }
  }
}

fn merge_pairs(l: List(Tree(a)), compare: fn(a, a) -> Order) -> Tree(a) {
  case l {
    [] -> Empty
    [h] -> h
    [h1, h2, ..hs] ->
      merge_trees(
        merge_trees(h1, h2, compare),
        merge_pairs(hs, compare),
        compare,
      )
  }
}
