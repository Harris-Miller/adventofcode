import { find, max, pipe } from 'ramda';

import { breadthFirstSearch } from '../lib/searchAlgorithms';

type Point = [number, number];

const strToPoint = (s: string) => s.split(', ').map(Number) as Point;
const pointToStr = ([r, c]: Point) => `${r}, ${c}`;

const gridReader = (pred: (v: string) => boolean, input: string) => {
  const l = input.length;
  let i = 0;
  let row = 0;
  let col = 0;

  const grid = new Map<string, string>();

  while (i < l) {
    const char = input.charAt(i);
    if (char === '\n') {
      row += 1;
      col = 0;
    } else {
      if (pred(char)) {
        grid.set(`${row}, ${col}`, char);
      }
      col += 1;
    }
    i += 1;
  }

  return grid;
};

const content = await Bun.file('../../inputs/2023/Day21/sample.txt').text();
const grid = gridReader(v => v === '#' || v === 'S', content);

const maxes = Array.from(grid.keys())
  .map(strToPoint)
  .reduce<Point>((acc, [r, c]) => [max(acc[0], r), max(acc[1], c)] as Point, [0, 0]);

const start = pipe(
  find<[string, string]>(([, v]) => v === 'S'),
  s => s![0],
)(Array.from(grid.entries()));

// the second item in this pair is the "depth" number that we'll use to determine the end of the search
type DepthPair = [string, number];

const isOutOfBounds = (value: string) => {
  const [r, c] = strToPoint(value);
  return r < 0 || r > maxes[0] || c < 0 || c > maxes[1];
};

const changeBys: [number, number][] = [
  [-1, 0],
  [0, 1],
  [1, 0],
  [0, -1],
];
const getChildren = ([value, depth]: DepthPair): DepthPair[] => {
  const [r, c] = strToPoint(value);
  return changeBys
    .map(([rc, cc]) => [r + rc, c + cc] as [number, number])
    .map(pointToStr)
    .filter(next => !grid.has(next) && !isOutOfBounds(next))
    .map(s => [s, depth + 1]);
};

const found =
  (maxDepth: number) =>
  ([, depth]: DepthPair): boolean =>
    depth === maxDepth;

const r = breadthFirstSearch(getChildren, found(64), [start, 0]);

console.log(r);
