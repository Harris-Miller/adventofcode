/* eslint-disable no-plusplus */
import * as R from 'ramda';

import { parseInt10 } from './fp';

export type Coord = [number, number];

export const coordsToString = (coord: Coord) => R.toString(coord);

export const stringToCoords = (str: string) => R.tail(R.init(str)).split(', ').map(parseInt10) as [number, number];

export type Grid = Map<string, string>;

export const collectGrid = (filter: (char: string) => boolean, input: string): Grid => {
  const grid = new Map<string, string>();
  const rows = input.split('\n');
  const rLen = rows.length;

  for (let r = 0; r < rLen; ++r) {
    const row = rows[r].split('');
    const len = row.length;
    for (let c = 0; c < len; ++c) {
      const val = row[c];
      if (filter(val)) {
        const coord = R.toString([r, c]);
        grid.set(coord, val);
      }
    }
  }

  return grid;
};

export const parseGridAsIs = (input: string): Grid => collectGrid(R.T, input);
