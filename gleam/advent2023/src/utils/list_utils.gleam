fn do_init(remaining: List(a)) -> List(a) {
  case remaining {
    [] -> []
    [_] -> []
    [x, ..xs] -> [x, ..do_init(xs)]
  }
}

pub fn init(list: List(a)) -> Result(List(a), Nil) {
  case list {
    [] -> Error(Nil)
    xs -> Ok(do_init(xs))
  }
}
