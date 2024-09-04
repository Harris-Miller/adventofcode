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

const drawRect = ([c, r]: [number, number], grid: Grid): Grid => {
  const points = createGrid(c, r);
  return points.reduce((acc, p) => {
    acc[R.toString(p)] = true;
    return acc;
  }, grid);
};

const rotateColumn = (col: number, by: number, grid: Grid): Grid => {
  const pointFrom = R.range(0, height).map<[number, number]>(r => [col, r]);
  const pointTo = R.range(0, height).map<[number, number]>(r => [col, (r + by) % height]);
  const updatedCol = R.zip(pointFrom, pointTo).reduce<Record<string, boolean>>((acc, [f, t]) => {
    acc[R.toString(t)] = grid[R.toString(f)];
    return acc;
  }, {});
  console.log(updatedCol);
  return Object.assign(grid, updatedCol);
};

const rotateRow = (row: number, by: number, grid: Grid): Grid => {
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
  if (inst.startsWith('rect')) {
    const point = R.drop(5, inst).split('x').map(parseInt10) as [number, number];
    return drawRect(point, grid);
  }

  if (inst.startsWith('rotate column')) {
    const [l, r] = R.drop(14, inst).split(' by ') as [string, string];
    const col = parseInt10(R.drop(2, l));
    const by = parseInt10(r);
    return rotateColumn(col, by, grid);
  }

  if (inst.startsWith('rotate row')) {
    const [l, r] = R.drop(11, inst).split(' by ') as [string, string];
    const row = parseInt10(R.drop(2, l));
    const by = parseInt10(r);
    return rotateRow(row, by, grid);
  }

  throw new Error('inst not implemented');
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
