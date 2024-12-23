import { Dict } from 'fp-search-algorithms';
import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';
import type { Point } from '../lib/gridRaw';
import { dictToString } from '../lib/gridRaw';

const content = (await Bun.file('../inputs/2024/Day14/input.txt').text()).trim();

// console.log(content);

type Robot = [p: Point, v: Point];

const parse = (line: string) => line.split(' ').map(s => R.drop(2, s).split(',').reverse().map(parseInt10)) as Robot;

const robots = content.split('\n').map(parse);

// console.log(robots);

// const rLen = 7;
// const cLen = 11;
const rLen = 103;
const cLen = 101;

const rUpTo = (rLen - 1) / 2;
const cUpTo = (cLen - 1) / 2;

const robotToGrid = (rs: Robot[]) =>
  rs.reduce((acc, [p]) => {
    const val = acc.get(p);
    acc.set(p, (val ?? 0) + 1);
    return acc;
  }, new Dict<Point, number>());

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
  const r = (p[0] + v[0]) % rLen;
  const c = (p[1] + v[1]) % cLen;
  return [[r < 0 ? rLen + r : r, c < 0 ? cLen + c : c], v];
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

const grid2 = new Dict(
  robotToGrid(moved)
    .entries()
    .map<[Point, string]>(([k, v]) => [k, v.toString()]),
);
const asString2 = dictToString(rLen - 1, cLen - 1, '.', grid2);
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
