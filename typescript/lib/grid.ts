import * as R from 'ramda';

import { parseInt10 } from './fp';

export type Coord = [number, number];

export const stringToCoord = (str: string) => R.tail(R.init(str)).split(', ').map(parseInt10) as [number, number];

export type Grid = Map<string, string>;

export const collectGrid = (filter: (char: string) => boolean, input: string): [[number, number], Grid] => {
  const grid = new Map<string, string>();
  const parsed = input.split('\n').map(line => line.split(''));
  const rLen = parsed.length;
  const cLen = parsed[0].length;

  for (let r = 0; r < rLen; ++r) {
    const row = parsed[r];
    for (let c = 0; c < cLen; ++c) {
      const val = row[c];
      if (filter(val)) {
        const coord = R.toString([r, c]);
        grid.set(coord, val);
      }
    }
  }

  return [[rLen - 1, cLen - 1], grid];
};

export const parseGridAsIs = (input: string): Grid => collectGrid(R.T, input)[1];

export const gridToString = (rMax: number, cMax: number, filler: string, grid: Grid) =>
  R.range(0, rMax + 1)
    .map(r =>
      R.range(0, cMax + 1)
        .map(c => grid.get(R.toString([r, c])) ?? filler)
        .join(''),
    )
    .join('\n');

export const gridAsIsToString = (grid: Grid) => {
  let rMax = 0;
  let cMax = 0;
  for (const [r, c] of grid.keys().map(stringToCoord)) {
    if (r > rMax) rMax = r;
    if (c > cMax) cMax = c;
  }
  return gridToString(rMax, cMax, ' ', grid);
};

export const createIsInRangeFunc =
  (rMax: number, cMax: number) =>
  ([r, c]: Coord) =>
    r >= 0 && r <= rMax && c >= 0 && c <= cMax;
