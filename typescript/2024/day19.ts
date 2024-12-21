import { depthFirstSearch } from 'fp-search-algorithms';
import * as R from 'ramda';

const content = (await Bun.file('../inputs/2024/Day19/input.txt').text()).trim();

const [towels, designs] = (() => {
  const [tLine, dLines] = content.split('\n\n');
  return [tLine.split(', '), dLines.split('\n')] as const;
})();

const found = (state: string) => state.length === 0;
const next = (state: string) => towels.filter(t => state.startsWith(t)).map(t => R.drop(t.length, state));

const canBeMade = (design: string) => {
  const r = depthFirstSearch(next, found, design);
  return R.isNotNil(r);
};

const r1 = designs.filter(canBeMade).length;

console.log(r1);

export const generateTreeBreadthFirst = function* <T>(getNextStates: (a: T) => T[], start: T): Generator<T> {
  let current: T;
  const queue = [start];

  while (queue.length) {
    current = queue.shift()!;
    yield current;
    const children = getNextStates(current);
    if (children.length) {
      queue.push(...children);
    }
  }
};

const r2 = R.sum(
  designs.map(
    design =>
      generateTreeBreadthFirst(next, design)
        .filter(visit => visit === '')
        .toArray().length,
  ),
);

console.log(r2);
