import gleam/int

pub fn parse_assert(string: String) -> Int {
  let assert Ok(val) = int.parse(string)
  val
}
