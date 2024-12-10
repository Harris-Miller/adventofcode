import * as R from 'ramda';

import { combinations2 } from '../lib/fp';
import type { Coord } from '../lib/grid';
import { collectGrid, createIsInRangeFunc, gridToString, stringToCoord } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day8/input.txt').text()).trim();

const [[rMax, cMax], grid] = collectGrid(v => v !== '.', R.identity, content);

const isInRange = createIsInRangeFunc(rMax, cMax);

// console.log(gridToString(rMax, cMax, '.', grid));

const uniqFrequencies = R.uniq(grid.values().toArray());

const freqToCoords = uniqFrequencies.reduce<Record<string, string[]>>((acc, freq) => {
  const coords = grid
    .entries()
    .filter(([, val]) => val === freq)
    .map(([coord]) => coord)
    .toArray();
  // eslint-disable-next-line no-param-reassign
  acc[freq] = coords;
  return acc;
}, {});

// console.log(freqToCoords);

const getPlacementOps =
  ([ar, ac]: Coord, [br, bc]: Coord, [rDelta, cDelta]: Coord) =>
  ([zr, zc]: Coord, multiplier: number): Coord[] => {
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

const antiNodes = Object.values(freqToCoords).reduce((acc, coords) => {
  const combos = combinations2(coords);
  const antiNodeCoords = combos.flatMap(([a, b]) => {
    const [ar, ac] = stringToCoord(a);
    const [br, bc] = stringToCoord(b);
    const dr = Math.abs(ar - br);
    const dc = Math.abs(ac - bc);

    const ops = getPlacementOps([ar, ac], [br, bc], [dr, dc]);
    const [aAntiNode] = ops([ar, ac], 1);
    const [, bAntiNode] = ops([br, bc], 1);
    return [aAntiNode, bAntiNode].filter(isInRange);
  });
  antiNodeCoords.forEach(c => acc.add(R.toString(c)));
  return acc;
}, new Set<string>());

console.log(antiNodes.size);

// const antiNodesGrid = Array.from(antiNodes).map<[string, string]>(c => [c, '#']);
// const updatedGrid = new Map<string, string>([...antiNodesGrid, ...grid]);
// console.log(gridToString(rMax, cMax, '.', updatedGrid));

const antiNodes2 = Object.values(freqToCoords).reduce((acc, coords) => {
  const combos = combinations2(coords);
  const antiNodeCoords = combos.flatMap(([a, b]) => {
    const [ar, ac] = stringToCoord(a);
    const [br, bc] = stringToCoord(b);
    const dr = Math.abs(ar - br);
    const dc = Math.abs(ac - bc);

    const ops = getPlacementOps([ar, ac], [br, bc], [dr, dc]);

    const result: Coord[] = [];
    let multiplier = 1;
    let temp: Coord[] = [...ops([ar, ac], multiplier), ...ops([br, bc], multiplier)].filter(isInRange);
    while (temp.length) {
      result.push(...temp);
      multiplier += 1;
      temp = [...ops([ar, ac], multiplier), ...ops([br, bc], multiplier)].filter(isInRange);
    }
    return result;
  });
  antiNodeCoords.forEach(c => acc.add(R.toString(c)));
  return acc;
}, new Set<string>());

console.log(antiNodes2.size);

// const antiNodesGrid2 = Array.from(antiNodes2).map<[string, string]>(c => [c, '#']);
// const updatedGrid2 = new Map<string, string>([...antiNodesGrid2, ...grid]);
// console.log(gridToString(rMax, cMax, '.', updatedGrid2));
