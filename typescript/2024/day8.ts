import { DSet } from 'fp-search-algorithms';
import * as R from 'ramda';

import { combinations2 } from '../lib/fp';
import type { Point } from '../lib/grid';
import { createIsInRangeFunc, getGridLengths, gridEntries, stringToGrid } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day8/input.txt').text()).trim();

const grid = stringToGrid(content);
const { rLen, cLen } = getGridLengths(grid);

// console.log([[rLen, cLen], grid]);

const isInRange = createIsInRangeFunc(rLen, cLen);

// console.log(gridToString(rMax, cMax, '.', grid));

const uniqFrequencies = new Set(
  gridEntries(grid)
    .map(([, v]) => v)
    .filter(v => v !== '.'),
)
  .values()
  .toArray();

console.log(uniqFrequencies);

const freqToPoints = uniqFrequencies.reduce<Record<string, Point[]>>((acc, freq) => {
  const points = gridEntries(grid)
    .filter(([, val]) => val === freq)
    .map(([coord]) => coord);
  // eslint-disable-next-line no-param-reassign
  acc[freq] = points;
  return acc;
}, {});

// console.log(freqToCoords);

const getPlacementOps =
  ([ar, ac]: Point, [br, bc]: Point, [rDelta, cDelta]: Point) =>
  ([zr, zc]: Point, multiplier: number): Point[] => {
    const rDeltaM = multiplier * rDelta;
    const cDeltaM = multiplier * cDelta;

    if (ar < br && ac < bc) {
      return [
        [zr - rDeltaM, zc - cDeltaM],
        [zr + rDeltaM, zc + cDeltaM],
      ];
    }
    if (ar > br && ar > br) {
      return [
        [zr + rDeltaM, zc + cDeltaM],
        [zr - rDeltaM, zc - cDeltaM],
      ];
    }
    if (ar < br && ac > bc) {
      return [
        [zr - rDeltaM, zc + cDeltaM],
        [zr + rDeltaM, zc - cDeltaM],
      ];
    }
    if (ar > br && ac > bc) {
      return [
        [zr + rDeltaM, zc + cDeltaM],
        [zr - rDeltaM, zc - cDeltaM],
      ];
    }
    throw new Error('getPlacementOps should be exhaustive, how did we get here?');
  };

const antiNodes = Object.values(freqToPoints).reduce((acc, points) => {
  const combos = combinations2(points);
  const antiNodeCoords = combos.flatMap(([[ar, ac], [br, bc]]) => {
    const dr = Math.abs(ar - br);
    const dc = Math.abs(ac - bc);

    const ops = getPlacementOps([ar, ac], [br, bc], [dr, dc]);
    const [aAntiNode] = ops([ar, ac], 1);
    const [, bAntiNode] = ops([br, bc], 1);
    return [aAntiNode, bAntiNode].filter(isInRange);
  });
  antiNodeCoords.forEach(c => acc.add(c));
  return acc;
}, new DSet<Point>());

console.log(antiNodes.size);

// const antiNodesGrid = Array.from(antiNodes).map<[string, string]>(c => [c, '#']);
// const updatedGrid = new Map<string, string>([...antiNodesGrid, ...grid]);
// console.log(gridToString(rMax, cMax, '.', updatedGrid));

const antiNodes2 = Object.values(freqToPoints).reduce((acc, coords) => {
  const combos = combinations2(coords);
  const antiNodeCoords = combos.flatMap(([[ar, ac], [br, bc]]) => {
    const dr = Math.abs(ar - br);
    const dc = Math.abs(ac - bc);

    const ops = getPlacementOps([ar, ac], [br, bc], [dr, dc]);

    const result: Point[] = [];
    let multiplier = 1;
    let temp: Point[] = [...ops([ar, ac], multiplier), ...ops([br, bc], multiplier)].filter(isInRange);
    while (temp.length) {
      result.push(...temp);
      multiplier += 1;
      temp = [...ops([ar, ac], multiplier), ...ops([br, bc], multiplier)].filter(isInRange);
    }
    return result;
  });
  antiNodeCoords.forEach(c => acc.add(c));
  return acc;
}, new DSet<Point>());

console.log(antiNodes2.size);

// const antiNodesGrid2 = Array.from(antiNodes2).map<[string, string]>(c => [c, '#']);
// const updatedGrid2 = new Map<string, string>([...antiNodesGrid2, ...grid]);
// console.log(gridToString(rMax, cMax, '.', updatedGrid2));
