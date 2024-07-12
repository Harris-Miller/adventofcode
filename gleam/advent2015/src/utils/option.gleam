import gleam/option.{type Option, Some}

pub fn assert_unwrap(option: Option(a)) -> a {
  let assert Some(a) = option
  a
}
