import common/list as listc
import common/result as resultc
import common/tuple
import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import search_algorithms/breadth_first
import simplifile

// const parse = (line: string): [number, number[]] => {
//   const [testValue, nums] = line.split(': ');
//   return [parseInt10(testValue), nums.split(' ').map(parseInt10)];
// };
fn parse(line: String) {
  let assert [test_value, nums] = string.split(line, ": ")
  let a = int.parse(test_value) |> resultc.unwrap_assert
  let b =
    string.split(nums, " ")
    |> list.map(int.parse)
    |> list.map(resultc.unwrap_assert)
    |> listc.with_index
  #(a, b)
}

// const process =
//   (ops: ((a: number, b: number) => number)[]) =>
//   ([testValue, values]: [number, number[]]) => {
//     const found = ([acc, idx]: State) => acc === testValue && idx === values.length - 1;
//     const next = ([acc, idx]: State): State[] => {
//       if (acc > testValue) return [];
//       const nextIdx = idx + 1;
//       if (nextIdx >= values.length) return [];
//       const value = values[nextIdx];
//       return ops.map<State>(fn => [fn(acc, value), nextIdx]);
//     };

//     const result = breadthFirstSearch(next, found, [values[0], 0]);
//     return result;
//   };
fn process(ops: List(fn(Int, Int) -> Int)) {
  fn(t: #(Int, List(#(Int, Int)))) {
    let #(test_value, values) = t

    let found = fn(state: #(Int, Int)) {
      let #(acc, idx) = state
      acc == test_value && idx == list.length(values) - 1
    }

    let next = fn(state: #(Int, Int)) {
      let #(acc, idx) = state
      use <- bool.guard(acc > test_value, [])
      let next_idx = idx + 1
      use value <- resultc.unwrap_guard(list.key_find(values, next_idx), [])
      list.map(ops, fn(f) { #(f(acc, value), next_idx) })
    }

    let assert Ok(#(_, init)) = list.first(values)

    let result = breadth_first.breadth_first_search(next, found, #(init, 0))
    result.is_ok(result)
  }
}

pub fn main() {
  let assert Ok(content) =
    simplifile.read(from: "../../inputs/2024/Day7/input.txt")
    |> result.map(string.trim_end)
    |> result.map(string.split(_, "\n"))

  let equations = list.map(content, parse)

  // io.debug(equations)

  // const r1 = R.sum(equations.filter(process([R.add, R.multiply])).map(a => a[0]));
  let r1 =
    equations
    |> list.filter(process([int.add, int.multiply]))
    |> list.map(tuple.fst)
    |> int.sum

  io.debug(r1)
}
