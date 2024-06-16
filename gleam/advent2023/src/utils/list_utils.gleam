import gleam/list

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

pub fn lookup(list: List(#(key, value)), to_find: key) -> Result(value, Nil) {
  list
  |> list.find_map(fn(kv) {
    let #(k, v) = kv
    case k {
      kk if kk == to_find -> Ok(v)
      _ -> Error(Nil)
    }
  })
}
