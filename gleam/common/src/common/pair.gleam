/// Converts a list of length 2 to a tuple of size 2. panics if passed a list of any other length
pub fn from_list2(list: List(a)) -> #(a, a) {
  let assert [a, b] = list
  #(a, b)
}
