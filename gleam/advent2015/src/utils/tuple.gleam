pub fn from_list2(list: List(a)) -> #(a, a) {
  let assert [a, b] = list
  #(a, b)
}
