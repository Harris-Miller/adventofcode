import { breadthFirstTraversal, Dict, DSet } from 'fp-search-algorithms';
import * as R from 'ramda';

import { getNeighbors4, getPoint, gridEntries, stringToGrid } from '../lib/gridRaw';
import type { Point } from '../lib/gridRaw';

const content = (await Bun.file('../inputs/2024/Day12/input.txt').text()).trim();

const grid = stringToGrid(content);

// console.log(grid);

const visited = new DSet<Point>();

const regions = new Dict<[string, Point], [Point, number][]>();

const findRegion = (pStart: Point, letter: string) => {
  const next = ([pCurrent, _]: [Point, number]): [Point, number][] =>
    getNeighbors4(pCurrent)
      .filter(p => getPoint(p, grid) === letter)
      .map(p => [p, 4 - getNeighbors4(p).filter(n => getPoint(n, grid) === letter).length]);

  const start: [Point, number] = [pStart, 4 - getNeighbors4(pStart).filter(n => getPoint(n, grid) === letter).length];

  const inRegion = breadthFirstTraversal(next, start)
    .map(([p]) => {
      visited.add(p[0]);
      return p;
    })
    .toArray();

  regions.set([letter, pStart], inRegion);
};

// to collect a region, find a new letter that we haven't visited
// letters can have multiple regions, so we key by `${letter}-${coord}`
// once we find a letter, BFS neighbors that are same letter until no more
// collecting for all visits so we we no to skip on continuing scan
// on visit, while checking neighbors, all sides that are not same letter have a fence,
// collect that as part of saving to region

for (const [point, letter] of gridEntries(grid)) {
  if (visited.has(point)) continue;
  findRegion(point, letter);
}

const r1 = R.sum(
  regions
    .entries()
    .toArray()
    .map(([, xs]) => xs.length * R.sum(xs.map(c => c[1]))),
);

console.log(r1);
