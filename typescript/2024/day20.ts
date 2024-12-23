import { dijkstra } from 'fp-search-algorithms';
import * as R from 'ramda';

import { findInGrid, getPoint, stringToGrid } from '../lib/gridRaw';
import type { Grid, Point } from '../lib/gridRaw';

const content = (await Bun.file('../inputs/2024/Day20/input.txt').text()).trim();

const grid = stringToGrid(content);

// console.log(gridToString(grid));

const start = findInGrid(val => val === 'S', grid)!;
const end = findInGrid(val => val === 'E', grid)!;

// console.log(start, end);

const getNeighbors4 = ([r, c]: Point): Point[] => [
  [r - 1, c],
  [r, c + 1],
  [r + 1, c],
  [r, c - 1],
];

const okVals = new Set(['.', 'S', 'E']);

const makeNext = (g: Grid) => (p: Point) => getNeighbors4(p).filter(([r, c]) => okVals.has(g[r][c]));

type Direction = 'down' | 'left' | 'right' | 'up';

const getDirection = (prev: Point, current: Point): Direction => {
  const [up, right, down, left] = getNeighbors4(current);
  if (R.equals(prev, up)) return 'down';
  if (R.equals(prev, right)) return 'left';
  if (R.equals(prev, down)) return 'up';
  if (R.equals(prev, left)) return 'right';
  throw new Error('getDirection :: prev is not neighbor of current');
};

const move = (dir: Direction, [r, c]: Point): Point => {
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
      throw new Error('move non-exhaustive');
  }
};

const result = dijkstra(makeNext(grid), R.always(1), R.equals(end), start);
const cost = result[0]!;
const path = result[1]!;

// console.log(cost);
// console.log(path);

// for all, look for nexts that are '#' with '.' or 'E' after, but not if those points are previous in the path array
const cheats = R.range(1, path.length).flatMap(i => {
  const point = path[i];
  return getNeighbors4(point)
    .filter(([r, c]) => grid[r][c] === '#')
    .map(n => {
      const dir = getDirection(point, n);
      const n2 = move(dir, n);
      if (okVals.has(getPoint(n2, grid)!)) {
        const cheatIndex = path.findIndex(R.equals(n2));
        if (cheatIndex > i) return n2;
      }
      return undefined;
    })
    .filter(R.isNotNil)
    .map(cheatPoint => {
      const pointIndex = path.findIndex(R.equals(point));
      const cheatIndex = path.findIndex(R.equals(cheatPoint));
      return path.length - (cheatIndex - pointIndex - 2);
    });
});

const r1 = R.flow(cheats, [
  cs => R.collectBy(x => x.toString(), cs),
  cs => cs.map(c => [c.length, path.length - c[0]] as const),
  cs => cs.filter(([, v]) => v >= 100),
  cs => cs.map(([k]) => k),
  R.sum,
]);

console.log(r1);
