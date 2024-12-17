import * as R from 'ramda';

import type { Coord, Grid } from '../lib/grid';
import { gridAsIsToString, parseGridAsIs, stringToCoord } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day15/sample.txt').text()).trim();

type Direction = '^' | '<' | '>' | 'v';

const parse = () => {
  const temp = content.split('\n\n');
  return [parseGridAsIs(temp[0]), temp[1].split('').filter(c => c !== '\n') as Direction[]] as const;
};

const [[, parsed], movements] = parse();

const warehouse = new Map(parsed);

// console.log(gridAsIsToString(warehouse));

const start = stringToCoord(warehouse.entries().find(([, val]) => val === '@')![0]);

const getNextPos = (dir: Direction, [r, c]: Coord): Coord => {
  switch (dir) {
    case '^':
      return [r - 1, c];
    case '>':
      return [r, c + 1];
    case 'v':
      return [r + 1, c];
    case '<':
      return [r, c - 1];
    default:
      throw new Error('non-exhaustive');
  }
};

const findFirstEmpty = (grid: Grid, dir: Direction, boxPos: Coord): Coord[] | undefined => {
  let pos = boxPos;
  let val = grid.get(R.toString(pos));
  const coords: Coord[] = [];
  while (R.isNotNil(val)) {
    if (val === '#') return undefined;
    coords.push(pos);
    if (val === '.') return coords;
    pos = getNextPos(dir, pos);
    val = grid.get(R.toString(pos));
  }
  return undefined;
};

const pushBoxes = (grid: Grid, coords: Coord[]): Grid => {
  const [first, ...rest] = coords;
  rest.forEach(c => grid.set(R.toString(c), 'O'));
  grid.set(R.toString(first), '.');
  return grid;
};

const tryMove = (grid: Grid, dir: Direction, current: Coord): [Grid, Coord] => {
  const nextPos = getNextPos(dir, current);
  const valAtNextPos = warehouse.get(R.toString(nextPos))!;

  switch (valAtNextPos) {
    case '#':
      return [grid, current];
    // @ts-expect-error
    case 'O': {
      const firstEmptySpace = findFirstEmpty(grid, dir, nextPos);
      if (R.isNil(firstEmptySpace)) return [grid, current];
      // eslint-disable-next-line no-param-reassign
      grid = pushBoxes(grid, firstEmptySpace);
      // let fall-through
    }
    // eslint-disable-next-line no-fallthrough
    default:
      grid.set(R.toString(current), '.').set(R.toString(nextPos), '@');
      return [grid, nextPos];
  }
};

const [endGrid] = movements.reduce<[Grid, Coord]>(
  ([grid, current], dir) => {
    const next = tryMove(grid, dir, current);
    // console.log(gridAsIsToString(next[0]));
    return next;
  },
  [warehouse, start],
);

const r1 = R.sum(
  endGrid
    .entries()
    .filter(([, val]) => val === 'O')
    .map(([cStr]) => {
      const [r, c] = stringToCoord(cStr);
      return r * 100 + c;
    })
    .toArray(),
);

console.log(r1);

const adjustedWarehouse = new Map(
  parsed.entries().flatMap(([cStr, val]): [string, string][] => {
    const [r, c] = stringToCoord(cStr);
    const c2 = c * 2;
    const n1 = R.toString([r, c2]);
    const n2 = R.toString([r, c2 + 1]);
    switch (val) {
      case '@':
        return [
          [n1, '@'],
          [n2, '.'],
        ];
      case 'O':
        return [
          [n1, '['],
          [n2, ']'],
        ];
      default:
        return [
          [n1, val],
          [n2, val],
        ];
    }
  }),
);

console.log(gridAsIsToString(adjustedWarehouse));
