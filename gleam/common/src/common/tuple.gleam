pub fn from_list2(list: List(a)) -> #(a, a) {
  let assert [a, b] = list
  #(a, b)
}

pub fn fst(tuple: #(a, b)) -> a {
  let #(a, _) = tuple
  a
}

pub fn snd(tuple: #(a, b)) -> b {
  let #(_, b) = tuple
  b
}
