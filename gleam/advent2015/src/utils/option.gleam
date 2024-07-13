import gleam/option.{type Option, Some}

pub fn upwrap_assert(option: Option(a)) -> a {
  let assert Some(a) = option
  a
}
