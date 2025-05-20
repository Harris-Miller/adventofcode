/* eslint-disable no-param-reassign */
import * as R from 'ramda';

import { pairs, parseInt10, permutations } from '../lib/fp';

const content = (await Bun.file('../inputs/2015/Day9/input.txt').text()).trim().split('\n');

type Edges = Record<string, Record<string, number>>;

const parse = (data: string[]): Edges => {
  return data.reduce<Edges>((acc, line) => {
    const [from, , to, , dist] = line.split(' ');
    if (R.isNil(acc[from])) {
      acc[from] = {};
    }

    if (R.isNil(acc[to])) {
      acc[to] = {};
    }
    const val = parseInt10(dist);

    acc[from][to] = val;
    acc[to][from] = val;
    return acc;
  }, {});
};

const edges = parse(content);

const locations = Object.keys(edges);
// console.log(locations);

const paths = permutations(locations);
// console.log(paths);

const result1 = paths.reduce((acc, path) => {
  const asPairs = pairs(path);
  const distances = asPairs.map(([to, from]) => edges[to][from]);
  const total = R.sum(distances);
  // console.log(asPairs, total);
  return total < acc ? total : acc;
}, Infinity);

console.log(result1);

const result2 = paths.reduce((acc, path) => {
  const asPairs = pairs(path);
  const distances = asPairs.map(([to, from]) => edges[to][from]);
  const total = R.sum(distances);
  // console.log(asPairs, total);
  return total > acc ? total : acc;
}, 0);

console.log(result2);
