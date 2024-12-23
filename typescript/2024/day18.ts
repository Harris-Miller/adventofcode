import { Dict, dijkstra } from 'fp-search-algorithms';
import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';
import type { Point } from '../lib/gridRaw';
import { getNeighbors4 } from '../lib/gridRaw';

const content = (await Bun.file('../inputs/2024/Day18/input.txt').text()).trim();

// console.log(content);

// const rMax = 6;
// const cMax = 6;
// const toDrop = 12;
const rMax = 70;
const cMax = 70;
const toDrop = 1024;

const processed = content.split('\n').map<Point>(line => {
  const [c, r] = line.split(',');
  return [parseInt10(r), parseInt10(c)];
});

const grid = new Dict<Point, string>();
for (let i = 0; i <= rMax; i += 1) {
  for (let j = 0; j <= cMax; j += 1) {
    grid.set([i, j], '.');
  }
}

R.take(toDrop, processed).forEach(c => {
  grid.set(c, '#');
});

// console.log(dictAsIsToString(grid));

const next = (coord: Point) => getNeighbors4(coord).filter(n => grid.get(n) === '.');
const found = (coord: Point) => R.equals(coord, [rMax, cMax]);

const r1 = dijkstra(next, R.always(1), found, [0, 0] as Point);

console.log((r1[1]?.length ?? 0) - 1);

const bytesLeft = R.drop(toDrop, processed);

console.log(bytesLeft.length);

let r2: Point | undefined;
const dynamicGrid = new Dict(grid.entries());
const next2 = (coord: Point) => getNeighbors4(coord).filter(n => dynamicGrid.get(n) === '.');

while (bytesLeft.length) {
  const nextByte = bytesLeft.shift()!;
  dynamicGrid.set(nextByte, '#');
  const r = dijkstra(next2, R.always(1), found, [0, 0] as Point);
  if (R.isNil(r[0])) {
    r2 = nextByte;
    break;
  }
}

console.log(r2);
