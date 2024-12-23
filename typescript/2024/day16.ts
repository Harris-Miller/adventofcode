import { dijkstraAssoc } from 'fp-search-algorithms';
import * as R from 'ramda';

import type { Direction, Point } from '../lib/gridRaw';
import { findInGrid, getPoint, stringToGrid } from '../lib/gridRaw';

type State = [dir: Direction, point: Point];

const content = (await Bun.file('../inputs/2024/Day16/sample2.txt').text()).trim();

const grid = stringToGrid(content);

// console.log(gridAsIsToString(grid));

const start = findInGrid(val => val === 'S', grid)!;
const end = findInGrid(val => val === 'E', grid)!;

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

const rotate = ([dir, point]: State): State[] => {
  switch (dir) {
    case 'up':
    case 'down':
      return [
        ['left', point],
        ['right', point],
      ];
    case 'right':
    case 'left':
      return [
        ['up', point],
        ['down', point],
      ];
    default:
      throw new Error('rotate non-exhaustive');
  }
};

const found = ([, point]: State): boolean => R.equals(point, end);

const next = (state: State): [State, number][] => {
  const n = move(state);
  const v = getPoint(n[1], grid);
  const ns: [State, number][] = R.isNotNil(v) && v !== '#' ? [[n, 1]] : [];
  const rs: [State, number][] = rotate(state).map(x => [x, 1000]);
  return [...ns, ...rs];
};

const r1 = dijkstraAssoc(next, found, ['right', start] as State);
const [r1Score] = r1;
console.log(r1Score);
