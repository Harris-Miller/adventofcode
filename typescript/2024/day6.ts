/* eslint-disable @typescript-eslint/no-loop-func */
import * as R from 'ramda';
import { match } from 'ts-pattern';

import type { Coord, Grid } from '../lib/grid';
import { coordsToString, parseGridAsIs, stringToCoords } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day6/input.txt').text()).trim();

// console.log(content);

type Direction = 'down' | 'left' | 'right' | 'up';

const turnRight = (dir: Direction): Direction =>
  match<Direction, Direction>(dir)
    .with('up', () => 'right')
    .with('right', () => 'down')
    .with('down', () => 'left')
    .with('left', () => 'up')
    .exhaustive();

const lookInDir = (getNext: (coord: Coord) => Coord, coord: Coord, grid: Grid): [Set<string>, Coord | undefined] => {
  const visits = new Set<string>();
  let current = coord;
  while (grid.has(coordsToString(current))) {
    const next: Coord = getNext(current);
    if (grid.get(coordsToString(next)) === '#') return [visits, current];
    visits.add(coordsToString(current));
    current = next;
  }
  return [visits, undefined];
};

const lookUp = (coord: Coord, grid: Grid) => lookInDir(([r, c]) => [r - 1, c], coord, grid);
const lookDown = (coord: Coord, grid: Grid) => lookInDir(([r, c]) => [r + 1, c], coord, grid);
const lookRight = (coord: Coord, grid: Grid) => lookInDir(([r, c]) => [r, c + 1], coord, grid);
const lookLeft = (coord: Coord, grid: Grid) => lookInDir(([r, c]) => [r, c - 1], coord, grid);

const grid = parseGridAsIs(content);

// console.log(grid);

let pos: Coord | undefined = stringToCoords(grid.entries().find(([, val]) => val === '^')![0]);

let dir: Direction = 'up';

let collection = new Set<string>();

while (R.isNotNil(pos)) {
  const [newVisits, nextPos] = match(dir)
    .with('up', () => lookUp(pos!, grid))
    .with('right', () => lookRight(pos!, grid))
    .with('down', () => lookDown(pos!, grid))
    .with('left', () => lookLeft(pos!, grid))
    .exhaustive();

  dir = turnRight(dir);
  pos = nextPos;
  collection = collection.union(newVisits);
}

console.log(collection.size);
