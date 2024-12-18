import { dijkstraAssoc } from 'fp-search-algorithms';
import * as R from 'ramda';

import type { Coord } from '../lib/grid';
import { parseGridAsIs, stringToCoord } from '../lib/grid';

type Direction = 'down' | 'left' | 'right' | 'up';

type State = [dir: Direction, coord: Coord];

const content = (await Bun.file('../inputs/2024/Day16/sample2.txt').text()).trim();

const [, grid] = parseGridAsIs(content);

// console.log(gridAsIsToString(grid));

const start = stringToCoord(grid.entries().find(([, val]) => val === 'S')![0]);
const end = stringToCoord(grid.entries().find(([, val]) => val === 'E')![0]);

// console.log(start, end);

const move = ([dir, [r, c]]: State): State => {
  switch (dir) {
    case 'up':
      return [dir, [r - 1, c]];
    case 'right':
      return [dir, [r, c + 1]];
    case 'down':
      return [dir, [r + 1, c]];
    case 'left':
      return [dir, [r, c - 1]];
    default:
      throw new Error('non-exhaustive');
  }
};

const rotate = ([dir, coord]: State): State[] => {
  switch (dir) {
    case 'up':
    case 'down':
      return [
        ['left', coord],
        ['right', coord],
      ];
    case 'right':
    case 'left':
      return [
        ['up', coord],
        ['down', coord],
      ];
    default:
      throw new Error('non-exhaustive');
  }
};

const found = ([, coord]: State): boolean => R.equals(coord, end);

const next = (state: State): [State, number][] => {
  const n = move(state);
  const v = grid.get(R.toString(n[1]));
  const ns: [State, number][] = R.isNotNil(v) && v !== '#' ? [[n, 1]] : [];
  const rs: [State, number][] = rotate(state).map(x => [x, 1000]);
  return [...ns, ...rs];
};

const r1 = dijkstraAssoc(next, found, ['right', start] as State);
const [r1Score] = r1;
console.log(r1Score);

// part 2 requires us to find _all_ paths with min score, not just first found
// to do this, we can't just use dijkstra, we need to generate a tree
// * each path only cares not to revisit on it's own path, not all paths
// * stop tree-gen when no neighbors
// * stop tree-gen when accumulated score goes beyond result in part1
// * stop tree-gen when `'E'` is found

type State2 = {
  pathTo: State[];
  score: number;
};

type Node = { children: Node[]; state: State2 };

const generateTree = (state2: State2): Node => {
  const current = R.last(state2.pathTo)!;
  if (found(current)) {
    return {
      children: [],
      state: state2,
    };
  }

  const ns = next(current)
    .filter(
      ([s]) =>
        R.equals(current[1], s[1]) ||
        !R.includes(
          s[1],
          state2.pathTo.map(x => x[1]),
        ),
    )
    .map<State2>(([s, score]) => ({ pathTo: R.append(s, state2.pathTo), score: state2.score + score }))
    .filter(s2 => s2.score <= r1Score!);

  return {
    children: ns.map(generateTree),
    state: state2,
  };
};

const tree = generateTree({ pathTo: [['right', start]], score: 0 });

/** simple tree traversal that doesn't care about multiple visits */
const traverse = function* (node: Node): Generator<State2> {
  yield node.state;
  for (const child of node.children) {
    yield* traverse(child);
  }
};

const r2 = new Set<string>();

for (const visit of traverse(tree)) {
  // console.log(visit.score);
  const current = R.last(visit.pathTo)!;
  if (found(current)) {
    visit.pathTo.forEach(([, coord]) => {
      r2.add(R.toString(coord));
    });
  }
}

console.log(r2.size);
