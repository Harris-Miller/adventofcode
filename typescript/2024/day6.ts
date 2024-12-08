import * as R from 'ramda';
import { match } from 'ts-pattern';

import type { Coord, Grid } from '../lib/grid';
import { parseGridAsIs, stringToCoord } from '../lib/grid';

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

const walkPath = function* (grid: Grid, startCoord: Coord, startDirection: Direction): Generator<[Coord, Direction]> {
  let coord = startCoord;
  let dir = startDirection;

  while (true) {
    yield [coord, dir];
    const nextCoord = getNextCoord(coord, dir);
    const nextSpace = grid.get(R.toString(nextCoord));

    // break out if we have left the grid
    if (R.isNil(nextSpace)) return;

    if (nextSpace === '#') {
      dir = turnRight(dir);
    } else {
      coord = nextCoord;
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

const obstructionsCoordsThatCauseLoops = new Set<string>();

for (const [cCoord, cDir] of walkPath(parsedGrid, startingCoord, startingDir)) {
  const nextCoord = getNextCoord(cCoord, cDir);
  const nStr = R.toString(nextCoord);

  // if we've already placed an obstruction here from a different direction, can skip
  if (obstructionsCoordsThatCauseLoops.has(nStr)) {
    continue;
  }

  // if nextSpace already _is_ an obstruction, or is out-of-bound (cannot place obstruction), move on
  const nextSpace = parsedGrid.get(nStr);
  if (nextSpace === '#' || R.isNil(nextSpace)) {
    continue;
  }

  const gridWithNewObstruction = new Map(parsedGrid);
  gridWithNewObstruction.set(nStr, '#');

  const loopStops = new Set<string>();
  // walk from starting point current coord turned do right
  for (const vector of walkPath(gridWithNewObstruction, cCoord, cDir)) {
    const lStr = R.toString(vector);
    // if we've already visited this spot moving in the same direction, we know we are in a loop
    if (loopStops.has(lStr)) {
      obstructionsCoordsThatCauseLoops.add(nStr);
      break;
    }
    loopStops.add(lStr);
  }
}

console.log(obstructionsCoordsThatCauseLoops.size);
