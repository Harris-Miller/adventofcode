import { Dict } from 'fp-search-algorithms';
import * as R from 'ramda';

import type { Grid, Point } from '../lib/gridRaw';
import { dictAsIsToString, findInGrid, getPoint, gridEntries, setInGrid, stringToGrid } from '../lib/gridRaw';

const content = (await Bun.file('../inputs/2024/Day15/sample.txt').text()).trim();

type Direction = '^' | '<' | '>' | 'v';

const parse = () => {
  const temp = content.split('\n\n');
  return [stringToGrid(temp[0]), temp[1].split('').filter(c => c !== '\n') as Direction[]] as const;
};

const [parsed, movements] = parse();

const warehouse: Grid = R.clone(parsed);

// console.log(gridAsIsToString(warehouse));

const start = findInGrid(val => val === '@', warehouse)!;

const getNextPos = (dir: Direction, [r, c]: Point): Point => {
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

const findFirstEmpty = (grid: Grid, dir: Direction, boxPoint: Point): Point[] | undefined => {
  let point = boxPoint;
  let val = getPoint(point, grid);
  const coords: Point[] = [];
  while (R.isNotNil(val)) {
    if (val === '#') return undefined;
    coords.push(point);
    if (val === '.') return coords;
    point = getNextPos(dir, point);
    val = getPoint(point, grid);
  }
  return undefined;
};

const pushBoxes = (grid: Grid, coords: Point[]): Grid => {
  const [first, ...rest] = coords;
  rest.forEach(c => setInGrid(c, 'O', grid));
  setInGrid(first, '.', grid);
  return grid;
};

const tryMove = (grid: Grid, dir: Direction, current: Point): [Grid, Point] => {
  const nextPoint = getNextPos(dir, current);
  const valAtNextPos = getPoint(nextPoint, warehouse)!;

  switch (valAtNextPos) {
    case '#':
      return [grid, current];
    // @ts-expect-error
    case 'O': {
      const firstEmptySpace = findFirstEmpty(grid, dir, nextPoint);
      if (R.isNil(firstEmptySpace)) return [grid, current];
      // eslint-disable-next-line no-param-reassign
      grid = pushBoxes(grid, firstEmptySpace);
      // let fall-through
    }
    // eslint-disable-next-line no-fallthrough
    default:
      setInGrid(nextPoint, '@', setInGrid(current, '.', grid));
      return [grid, nextPoint];
  }
};

const [endGrid] = movements.reduce<[Grid, Point]>(
  ([grid, current], dir) => {
    const next = tryMove(grid, dir, current);
    // console.log(gridAsIsToString(next[0]));
    return next;
  },
  [warehouse, start],
);

const r1 = R.flow(endGrid, [
  gridEntries<string>,
  es => es.filter(([, val]) => val === 'O'),
  es => es.map(([[r, c]]) => r * 100 + c),
  R.sum,
]);

console.log(r1);

const adjustedWarehouse = new Dict<Point, string>(
  gridEntries(parsed).flatMap(([[r, c], val]): [Point, string][] => {
    const c2 = c * 2;
    const n1: Point = [r, c2];
    const n2: Point = [r, c2 + 1];

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

console.log(dictAsIsToString(adjustedWarehouse));
