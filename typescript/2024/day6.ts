import * as R from 'ramda';

import type { Coord, Grid } from '../lib/grid';
import { parseGridAsIs, stringToCoord } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day6/input.txt').text()).trim();

// console.log(content);

type Direction = 'down' | 'left' | 'right' | 'up';

const getNextCoord = ([r, c]: Coord, dir: Direction): Coord => {
  switch (dir) {
    case 'up':
      return [r - 1, c];
    case 'right':
      return [r, c + 1];
    case 'down':
      return [r + 1, c];
    case 'left':
      return [r, c - 1];
    default:
      throw new Error('getNextCoord not Exhaustive');
  }
};

const turnRight = (dir: Direction): Direction => {
  switch (dir) {
    case 'up':
      return 'right';
    case 'right':
      return 'down';
    case 'down':
      return 'left';
    case 'left':
      return 'up';
    default:
      throw new Error('turnRight not Exhaustive');
  }
};

const walkPath = function* (grid: Grid, startCoord: Coord, startDirection: Direction): Generator<[Coord, Direction]> {
  let coord = startCoord;
  let dir = startDirection;
  let space = grid.get(R.toString(coord));

  while (R.isNotNil(space)) {
    yield [coord, dir];
    const nextCoord = getNextCoord(coord, dir);
    const nextSpace = grid.get(R.toString(nextCoord));

    if (nextSpace === '#') {
      dir = turnRight(dir);
    } else {
      coord = nextCoord;
      space = nextSpace;
    }
  }
};

const parsedGrid = parseGridAsIs(content);

const startingCoord: Coord = stringToCoord(parsedGrid.entries().find(([, val]) => val === '^')![0]);
const startingDir: Direction = 'up';
const r1 = new Set(walkPath(parsedGrid, startingCoord, startingDir).map(([coord]) => R.toString(coord)));

console.log(r1.size);

//
// part 2 brute force
// I'm sure there is a better way, but fuck it
//

// don't count starting position
const guardPath = R.tail(Array.from(r1));

const r2 = guardPath
  .map(cStr => new Map(parsedGrid).set(cStr, '#'))
  .filter(gridWithNewObstruction => {
    const history = new Set<string>();
    for (const vector of walkPath(gridWithNewObstruction, startingCoord, startingDir)) {
      const vStr = R.toString(vector);
      if (history.has(vStr)) return true;
      history.add(vStr);
    }
    return false;
  }).length;

console.log(r2);
