import gleam/option.{type Option, Some}

pub fn unwrap_assert(option: Option(a)) -> a {
  let assert Some(a) = option
  a
}
