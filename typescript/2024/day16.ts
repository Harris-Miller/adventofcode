import { dijkstraAssoc } from 'fp-search-algorithms';
import * as R from 'ramda';

import type { Coord } from '../lib/grid';
import { parseGridAsIs, stringToCoord } from '../lib/grid';

type Direction = 'down' | 'left' | 'right' | 'up';

type State = [dir: Direction | undefined, coord: Coord];

const content = (await Bun.file('../inputs/2024/Day16/input.txt').text()).trim();

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

console.log(r1[0]);
