import { breadthFirstSearch } from 'fp-search-algorithms';
import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';

const content = (await Bun.file('../inputs/2024/Day7/input.txt').text()).trim();

const parse = (line: string): [number, number[]] => {
  const [testValue, nums] = line.split(': ');
  return [parseInt10(testValue), nums.split(' ').map(parseInt10)];
};

const equations = content.split('\n').map(parse);

// console.log(equations);

type State = [acc: number, i: number];

const process =
  (ops: ((a: number, b: number) => number)[]) =>
  ([testValue, values]: [number, number[]]) => {
    const found = ([acc, idx]: State) => acc === testValue && idx === values.length - 1;
    const next = ([acc, idx]: State): State[] => {
      if (acc > testValue) return [];
      const nextIdx = idx + 1;
      if (nextIdx >= values.length) return [];
      const value = values[nextIdx];
      return ops.map<State>(fn => [fn(acc, value), nextIdx]);
    };

    const result = breadthFirstSearch(next, found, [values[0], 0]);
    return result;
  };

const r1 = R.sum(equations.filter(process([R.add, R.multiply])).map(a => a[0]));

console.log(r1);

const r2 = R.sum(equations.filter(process([R.add, R.multiply, (a, b) => parseInt10(`${a}${b}`)])).map(a => a[0]));

console.log(r2);
