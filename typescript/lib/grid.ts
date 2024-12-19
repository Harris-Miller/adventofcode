import type { Dict } from 'fp-search-algorithms';
import * as R from 'ramda';

import { parseInt10 } from './fp';

export type Coord = [row: number, col: number];

export const stringToCoord = (str: string) => R.tail(R.init(str)).split(', ').map(parseInt10) as [number, number];

export type Grid<T = string> = Map<string, T>;

export const collectGrid = <T>(
  filter: (char: string) => boolean,
  parse: (char: string) => T,
  input: string,
): [[rMax: number, cMax: number], grid: Grid<T>] => {
  const grid = new Map<string, T>();
  const parsed = input.split('\n').map(line => line.split(''));
  const rLen = parsed.length;
  const cLen = parsed[0].length;

  for (let r = 0; r < rLen; ++r) {
    const row = parsed[r];
    for (let c = 0; c < cLen; ++c) {
      const val = row[c];
      if (filter(val)) {
        const coord = R.toString([r, c]);
        grid.set(coord, parse(val));
      }
    }
  }

  return [[rLen - 1, cLen - 1], grid];
};

export const parseGridAsIs = (input: string) => collectGrid(R.T, R.identity, input);

export const dictToString = (rMax: number, cMax: number, filler: string, dict: Dict<Coord, string>) =>
  R.range(0, rMax + 1)
    .map(r =>
      R.range(0, cMax + 1)
        .map(c => dict.get([r, c]) ?? filler)
        .join(''),
    )
    .join('\n');

export const gridToString = (rMax: number, cMax: number, filler: string, grid: Grid) =>
  R.range(0, rMax + 1)
    .map(r =>
      R.range(0, cMax + 1)
        .map(c => grid.get(R.toString([r, c])) ?? filler)
        .join(''),
    )
    .join('\n');

export const dictAsIsToString = (dict: Dict<Coord, string>) => {
  let rMax = 0;
  let cMax = 0;
  for (const [r, c] of dict.keys()) {
    if (r > rMax) rMax = r;
    if (c > cMax) cMax = c;
  }
  return dictToString(rMax, cMax, ' ', dict);
};

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

export const getNeighbors4 = (coord: Coord | string): Coord[] => {
  if (typeof coord === 'string') {
    // eslint-disable-next-line no-param-reassign
    coord = stringToCoord(coord);
  }

  const [r, c] = coord;

  return [
    [r - 1, c],
    [r, c + 1],
    [r + 1, c],
    [r, c - 1],
  ];
};
