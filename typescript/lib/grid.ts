import type { HashMap } from 'fp-search-algorithms';
import * as R from 'ramda';

export type Grid<T = string> = T[][];
export type Point = [row: number, col: number];
export type Direction = 'down' | 'left' | 'right' | 'up';

export const parseToGrid = <T>(parse: (str: string) => T, str: string): Grid<T> =>
  str.split('\n').map(line => line.split('').map(parse));
export const stringToGrid = (str: string): Grid => parseToGrid(x => x, str);
export const gridToString = (grid: Grid): string => grid.map(line => line.join('')).join('\n');

export const getGridLengths = (grid: Grid<unknown>) => ({
  cLen: grid[0].length,
  rLen: grid.length,
});

export const getPoint = <T>([r, c]: Point, grid: Grid<T>): T | undefined => grid[r]?.[c];

export const setInGrid = ([r, c]: Point, val: string, grid: Grid): Grid => {
  // eslint-disable-next-line no-param-reassign
  grid[r][c] = val;
  return grid;
};

export const findInGrid = (predicate: (val: string, point: Point) => boolean, grid: Grid): Point | undefined => {
  const { cLen: cLength, rLen: rLength } = getGridLengths(grid);
  for (let r = 0; r < rLength; ++r) {
    for (let c = 0; c < cLength; ++c) {
      if (predicate(grid[r][c], [r, c])) {
        return [r, c];
      }
    }
  }
  return undefined;
};

export const gridEntries = <T>(grid: Grid<T>): [point: Point, val: T][] => {
  const entries = new Array<[point: Point, val: T]>(grid.length * grid[0].length);
  let i = 0;
  const { cLen: cLength, rLen: rLength } = getGridLengths(grid);

  for (let r = 0; r < rLength; ++r) {
    for (let c = 0; c < cLength; ++c) {
      entries[i] = [[r, c], grid[r]?.[c]];
      i++;
    }
  }

  return entries;
};

export const getNeighbors4 = ([r, c]: Point): Point[] => [
  [r - 1, c],
  [r, c + 1],
  [r + 1, c],
  [r, c - 1],
];

export const createIsInRangeFunc =
  (rLen: number, cLen: number) =>
  ([r, c]: Point) =>
    r >= 0 && r < rLen && c >= 0 && c < cLen;

export const getDirection = (prev: Point, current: Point): Direction => {
  const [up, right, down, left] = getNeighbors4(current);
  if (R.equals(prev, up)) return 'down';
  if (R.equals(prev, right)) return 'left';
  if (R.equals(prev, down)) return 'up';
  if (R.equals(prev, left)) return 'right';
  throw new Error('getDirection :: prev is not neighbor of current');
};

export const getNextPoint = ([r, c]: Point, dir: Direction): Point => {
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
      throw new Error('getNextPoint not Exhaustive');
  }
};

export const turnRight = (dir: Direction): Direction => {
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

export const turnLeft = (dir: Direction): Direction => {
  switch (dir) {
    case 'up':
      return 'left';
    case 'right':
      return 'up';
    case 'down':
      return 'right';
    case 'left':
      return 'down';
    default:
      throw new Error('turnRight not Exhaustive');
  }
};

export const dictToString = (rMax: number, cMax: number, filler: string, dict: HashMap<Point, string>) =>
  R.range(0, rMax + 1)
    .map(r =>
      R.range(0, cMax + 1)
        .map(c => dict.get([r, c]) ?? filler)
        .join(''),
    )
    .join('\n');

export const dictAsIsToString = (dict: HashMap<Point, string>) => {
  let rMax = 0;
  let cMax = 0;
  for (const [r, c] of dict.keys()) {
    if (r > rMax) rMax = r;
    if (c > cMax) cMax = c;
  }
  return dictToString(rMax, cMax, ' ', dict);
};
