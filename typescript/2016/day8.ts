/* eslint-disable no-param-reassign */
import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';

const content = (await Bun.file('../inputs/2016/Day8/input.txt').text()).trim().split('\n');

// console.log(content);

type Grid = Record<string, boolean>;

// sample 7 wide, 3 tall
// input  50 wide, 6 tall
const width = 50;
const height = 6;

const createGrid = (c: number, r: number): [number, number][] => {
  const cs = R.range(0, c);
  const rs = R.range(0, r);
  return R.xprod(cs, rs);
};

const drawRect = (inst: string, grid: Grid): Grid => {
  const [c, r] = R.drop(5, inst).split('x').map(parseInt10) as [number, number];
  const points = createGrid(c, r);
  return points.reduce((acc, p) => {
    acc[R.toString(p)] = true;
    return acc;
  }, grid);
};

const rotateColumn = (inst: string, grid: Grid): Grid => {
  const parsed = R.drop(14, inst).split(' by ') as [string, string];
  const col = parseInt10(R.drop(2, parsed[0]));
  const by = parseInt10(parsed[1]);
  const pointFrom = R.range(0, height).map<[number, number]>(r => [col, r]);
  const pointTo = R.range(0, height).map<[number, number]>(r => [col, (r + by) % height]);
  const updatedCol = R.zip(pointFrom, pointTo).reduce<Record<string, boolean>>((acc, [f, t]) => {
    acc[R.toString(t)] = grid[R.toString(f)];
    return acc;
  }, {});
  return Object.assign(grid, updatedCol);
};

const rotateRow = (inst: string, grid: Grid): Grid => {
  const parsed = R.drop(11, inst).split(' by ') as [string, string];
  const row = parseInt10(R.drop(2, parsed[0]));
  const by = parseInt10(parsed[1]);
  const pointFrom = R.range(0, width).map<[number, number]>(c => [c, row]);
  const pointTo = R.range(0, width).map<[number, number]>(c => [(c + by) % width, row]);
  const updatedRow = R.zip(pointFrom, pointTo).reduce<Record<string, boolean>>((acc, [f, t]) => {
    acc[R.toString(t)] = grid[R.toString(f)];
    return acc;
  }, {});
  console.log(updatedRow);
  return Object.assign(grid, updatedRow);
};

const processInst = (grid: Grid, inst: string): Grid => {
  switch (true) {
    case inst.startsWith('rect'):
      return drawRect(inst, grid);
    case inst.startsWith('rotate column'):
      return rotateColumn(inst, grid);
    case inst.startsWith('rotate row'):
      return rotateRow(inst, grid);
    default:
      throw new Error('inst not implemented');
  }
};

const gridToString = (grid: Grid): string =>
  R.range(0, height)
    .map(r =>
      R.range(0, width)
        .map(c => [c, r])
        .map(p => (grid[R.toString(p)] ? '#' : '.'))
        .join(''),
    )
    .join('\n');

const grid = createGrid(width, height).reduce<Record<string, boolean>>((acc, p) => {
  acc[R.toString(p)] = false;
  return acc;
}, {});

// console.log(gridToString(grid));

const r1 = content.reduce<Record<string, boolean>>(processInst, grid);

console.log(Object.values(r1).filter(x => x).length);

console.log(gridToString(r1));
