import gleam/option.{type Option, Some}

/// Unwrap an Option with assert, panics when `None`.
pub fn unwrap_assert(option: Option(a)) -> a {
  let assert Some(a) = option
  a
}
