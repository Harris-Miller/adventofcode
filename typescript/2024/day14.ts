// import { depthFirstSearch } from 'fp-search-algorithms';
import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';
import { gridToString } from '../lib/grid';
import type { Coord } from '../lib/grid';

// import { parseInt10 } from '../lib/fp';
// import type { Coord } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day14/input.txt').text()).trim();

// console.log(content);

type Robot = [p: Coord, v: Coord];

const parse = (line: string) => line.split(' ').map(s => R.drop(2, s).split(',').reverse().map(parseInt10)) as Robot;

const robots = content.split('\n').map(parse);

// console.log(robots);

// const rMax = 7;
// const cMax = 11;
const rMax = 103;
const cMax = 101;

const rUpTo = (rMax - 1) / 2;
const cUpTo = (cMax - 1) / 2;

const robotToGrid = (rs: Robot[]) =>
  new Map(
    rs
      .map(([p]) => R.toString(p))
      .reduce((acc, p) => {
        const val = acc.get(p);
        acc.set(p, (val ?? 0) + 1);
        return acc;
      }, new Map<string, number>())
      .entries()
      .map<[string, string]>(([k, v]) => [k, v.toString()]),
  );

const sample: Robot[] = [
  [
    [4, 2],
    [-3, 2],
  ],
];

// const grid = robotToGrid(sample);
// const asString = gridToString(rMax - 1, cMax - 1, '.', grid);
// console.log(asString);
// console.log('');

const move = ([p, v]: Robot): Robot => {
  const r = (p[0] + v[0]) % rMax;
  const c = (p[1] + v[1]) % cMax;
  return [[r < 0 ? rMax + r : r, c < 0 ? cMax + c : c], v];
};

const moved = R.range(0, 100)
  .reduce(acc => {
    const next = acc.map(move);
    // console.log(gridToString(rMax - 1, cMax - 1, '.', robotToGrid(next)));
    // console.log('');
    return next;
  }, robots)
  .filter(([[r, c]]) => !(r === rUpTo || c === cUpTo));

// console.log(moved);

const grid2 = robotToGrid(moved);
const asString2 = gridToString(rMax - 1, cMax - 1, '.', grid2);
console.log(asString2);

const quads = R.collectBy((robot: Robot) => {
  const [[r, c]] = robot;
  if (r < rUpTo && c < cUpTo) return '1';
  if (r < rUpTo && c >= cUpTo) return '2';
  if (r >= rUpTo && c < cUpTo) return '3';
  if (r >= rUpTo && c >= cUpTo) return '4';
  throw new Error('exhaustive');
}, moved);

console.log(quads.map(q => q.length));
const r1 = R.product(quads.map(q => q.length));
console.log(r1);
