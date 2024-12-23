export type Grid = string[][];
export type Point = [row: number, col: number];

export const stringToGrid = (str: string): Grid => str.split('\n').map(line => line.split(''));
export const gridToString = (grid: Grid): string => grid.map(line => line.join('')).join('\n');

export const getPoint = ([r, c]: Point, grid: Grid): string | undefined => grid[r]?.[c];

export const getGridLengths = (grid: Grid) => ({
  cLength: grid[0].length,
  rLength: grid.length,
});

export const findInGrid = (predicate: (val: string, point: Point) => boolean, grid: Grid): Point | undefined => {
  const { cLength, rLength } = getGridLengths(grid);
  for (let r = 0; r < rLength; ++r) {
    for (let c = 0; c < cLength; ++c) {
      if (predicate(grid[r][c], [r, c])) {
        return [r, c];
      }
    }
  }
  return undefined;
};
