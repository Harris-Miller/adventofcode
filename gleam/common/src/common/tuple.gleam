/// Converts a list of length 2 to a tuple of size 2. panics if passed a list of any other length
pub fn from_list2(list: List(a)) -> #(a, a) {
  let assert [a, b] = list
  #(a, b)
}

// returns the first value in a tuple
pub fn fst(tuple: #(a, b)) -> a {
  let #(a, _) = tuple
  a
}

// returns the second value in a tuple
pub fn snd(tuple: #(a, b)) -> b {
  let #(_, b) = tuple
  b
}
