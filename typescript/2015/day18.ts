import { getNeighbors8 } from '../lib/grid';

type Grid<T> = T[][];

const content = (await Bun.file('../inputs/2015/Day18/input.txt').text()).trim();

// console.log(content);

const startingGrid: Grid<string> = content.split('\n').map(line => line.split(''));

// console.log(startingGrid);

const process = (grid: Grid<string>): Grid<string> => {
  return grid.map((rows, r) =>
    rows.map((_row, c) => {
      const neighborsOn = getNeighbors8([r, c])
        .map(([nr, nc]) => grid[nr]?.[nc])
        .filter(char => char === '#').length;
      const current = grid[r][c];
      if (current === '#') {
        return neighborsOn === 2 || neighborsOn === 3 ? '#' : '.';
      }
      return neighborsOn === 3 ? '#' : '.';
    }),
  );
};

const nextGrid = Array<undefined>(100)
  .fill(undefined)
  .reduce(grid => process(grid), startingGrid);

const r1 = nextGrid.flat().filter(char => char === '#').length;
console.log(r1);
