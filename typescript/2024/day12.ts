import { breadthFirstTraversal } from 'fp-search-algorithms';
import * as R from 'ramda';

import { getNeighbors4, parseGridAsIs } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day12/input.txt').text()).trim();

const [, grid] = parseGridAsIs(content);

// console.log(grid);

const visited = new Set<string>();

const regions = new Map<string, [string, number][]>();

const findRegion = (cStart: string, letter: string) => {
  const next = ([cCurrent, _]: [string, number]): [string, number][] =>
    getNeighbors4(cCurrent)
      .map(c => R.toString(c))
      .filter(c => grid.get(c) === letter)
      .map(c => [c, 4 - getNeighbors4(c).filter(n => grid.get(R.toString(n)) === letter).length]);

  const start: [string, number] = [
    cStart,
    4 - getNeighbors4(cStart).filter(n => grid.get(R.toString(n)) === letter).length,
  ];

  const inRegion = breadthFirstTraversal(next, start)
    .map(([c]) => {
      visited.add(c[0]);
      return c;
    })
    .toArray();

  regions.set(`${letter}-${cStart}`, inRegion);
};

// to collect a region, find a new letter that we haven't visited
// letters can have multiple regions, so we key by `${letter}-${coord}`
// once we find a letter, BFS neighbors that are same letter until no more
// collecting for all visits so we we no to skip on continuing scan
// on visit, while checking neighbors, all sides that are not same letter have a fence,
// collect that as part of saving to region

for (const [coord, letter] of grid.entries()) {
  if (visited.has(coord)) continue;
  findRegion(coord, letter);
}

const r1 = R.sum(
  regions
    .entries()
    .toArray()
    .map(([, xs]) => xs.length * R.sum(xs.map(c => c[1]))),
);

console.log(r1);
