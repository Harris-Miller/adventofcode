import * as R from 'ramda';
import { match } from 'ts-pattern';

import type { Coord, Grid } from '../lib/grid';
import { coordToString, parseGridAsIs, stringToCoord } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day6/input.txt').text()).trim();

// console.log(content);

type Direction = 'down' | 'left' | 'right' | 'up';

const getNextCoord = ([r, c]: Coord, dir: Direction) =>
  match<Direction, Coord>(dir)
    .with('up', () => [r - 1, c])
    .with('right', () => [r, c + 1])
    .with('down', () => [r + 1, c])
    .with('left', () => [r, c - 1])
    .exhaustive();

const turnRight = (dir: Direction): Direction =>
  match<Direction, Direction>(dir)
    .with('up', () => 'right')
    .with('right', () => 'down')
    .with('down', () => 'left')
    .with('left', () => 'up')
    .exhaustive();

const walkPath = function* (grid: Grid, startCoord: Coord, startDirection: Direction): Generator<string> {
  let coord = startCoord;
  let dir = startDirection;

  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  while (true) {
    yield coordToString(coord);
    const next = getNextCoord(coord, dir);
    const nextSpace = grid.get(coordToString(next));
    if (R.isNil(nextSpace)) return;
    if (nextSpace === '#') {
      dir = turnRight(dir);
    }
    coord = getNextCoord(coord, dir);
  }
};

const parsedGrid = parseGridAsIs(content);

const startingCoord: Coord = stringToCoord(parsedGrid.entries().find(([, val]) => val === '^')![0]);

const startingDir: Direction = 'up';

// let collection = new Set<string>();

// while (R.isNotNil(pos)) {
//   const [newVisits, nextPos] = runPath(grid, dir, pos);

//   dir = turnRight(dir);
//   pos = nextPos;
//   collection = collection.union(newVisits);
// }

const r1 = new Set(walkPath(parsedGrid, startingCoord, startingDir));

console.log(r1.size);
