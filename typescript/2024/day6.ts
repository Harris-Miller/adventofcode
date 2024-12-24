import { HashSet } from 'fp-search-algorithms';
import * as R from 'ramda';

import type { Direction, Grid, Point } from '../lib/grid';
import { findInGrid, getNextPoint, getPoint, setInGrid, stringToGrid, turnRight } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day6/input.txt').text()).trim();

// console.log(content);

const walkPath = function* (grid: Grid, startPoint: Point, startDirection: Direction): Generator<[Point, Direction]> {
  let point = startPoint;
  let dir = startDirection;
  let space = getPoint(point, grid);

  while (R.isNotNil(space)) {
    yield [point, dir];
    const nextPoint = getNextPoint(point, dir);
    const nextSpace = getPoint(nextPoint, grid);

    if (nextSpace === '#') {
      dir = turnRight(dir);
    } else {
      point = nextPoint;
      space = nextSpace;
    }
  }
};

const parsedGrid = stringToGrid(content);

const startingCoord: Point = findInGrid(([val]) => val === '^', parsedGrid)!;
const startingDir: Direction = 'up';
const r1 = new HashSet(walkPath(parsedGrid, startingCoord, startingDir).map(([coord]) => coord));

console.log(r1.size);

//
// part 2 brute force
// I'm sure there is a better way, but fuck it
//

// don't count starting position
const guardPath = R.tail(Array.from(r1));

const r2 = guardPath
  .map(p => setInGrid(p, '#', R.clone(parsedGrid)))
  .filter(gridWithNewObstruction => {
    const history = new HashSet<[Point, Direction]>();
    for (const vector of walkPath(gridWithNewObstruction, startingCoord, startingDir)) {
      if (history.has(vector)) return true;
      history.add(vector);
    }
    return false;
  }).length;

console.log(r2 + 1);
