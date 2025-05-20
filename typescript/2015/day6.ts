import * as R from 'ramda';
import { match } from 'ts-pattern';

import { parseInt10 } from '../lib/fp';

type Grid<T> = T[][];

const startingGrid: Grid<boolean> = Array(1000)
  .fill(undefined)
  .map(() => Array(1000).fill(false) as boolean[]);

const content = (await Bun.file('../inputs/2015/Day6/input.txt').text()).trim().split('\n');

type Mode = 'off' | 'on' | 'toggle';

type Instruction = {
  end: [c: number, r: number];
  mode: Mode;
  start: [c: number, r: number];
};

const parseLine = (line: string): Instruction => {
  const words = line.split(' ');

  if (words[0] === 'toggle') {
    return {
      end: words[3].split(',').map(parseInt10) as [number, number],
      mode: 'toggle',
      start: words[1].split(',').map(parseInt10) as [number, number],
    };
  }

  return {
    end: words[4].split(',').map(parseInt10) as [number, number],
    mode: words[1] as Mode,
    start: words[2].split(',').map(parseInt10) as [number, number],
  };
};

const instructions = content.map(parseLine);

const process = (instruction: Instruction, grid: Grid<boolean>): Grid<boolean> => {
  const cs = R.range(instruction.start[0], instruction.end[0] + 1);
  const rs = R.range(instruction.start[1], instruction.end[1] + 1);

  return cs.reduce(
    (acc1, c) =>
      rs.reduce((acc2, r) => {
        const val = match(instruction.mode)
          .with('toggle', () => !acc2[c][r])
          .with('off', () => false)
          .with('on', () => true)
          .exhaustive();

        // eslint-disable-next-line no-param-reassign
        acc2[c][r] = val;
        return acc2;
      }, acc1),
    grid,
  );
};

const r1 = instructions
  .reduce((acc, inst) => process(inst, acc), startingGrid)
  .flat()
  .filter(Boolean);

console.log(r1.length);

const startingGrid2: Grid<number> = Array(1000)
  .fill(undefined)
  .map(() => Array(1000).fill(0) as number[]);

const process2 = (instruction: Instruction, grid: Grid<number>): Grid<number> => {
  const cs = R.range(instruction.start[0], instruction.end[0] + 1);
  const rs = R.range(instruction.start[1], instruction.end[1] + 1);

  return cs.reduce(
    (acc1, c) =>
      rs.reduce((acc2, r) => {
        const val = match(instruction.mode)
          .with('toggle', () => acc2[c][r] + 2)
          .with('off', () => {
            const currentValue = acc2[c][r];
            if (currentValue === 0) return 0;
            return currentValue - 1;
          })
          .with('on', () => acc2[c][r] + 1)
          .exhaustive();

        // eslint-disable-next-line no-param-reassign
        acc2[c][r] = val;
        return acc2;
      }, acc1),
    grid,
  );
};

const r2 = instructions
  .reduce((acc, inst) => process2(inst, acc), startingGrid2)
  .flat()
  .reduce((a, b) => a + b, 0);

console.log(r2);
