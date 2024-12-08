import * as R from 'ramda';

import { combinations2 } from '../lib/fp';
import type { Coord } from '../lib/grid';
import { collectGrid, createIsInRangeFunc, stringToCoord } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day8/input.txt').text()).trim();

const [[rMax, cMax], grid] = collectGrid(v => v !== '.', content);

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
  ([ar, ac]: Coord, [br, bc]: Coord) =>
  ([rDelta, cDelta]: Coord): Coord[] => {
    if (ar < br && ac < bc) {
      return [
        [ar - rDelta, ac - cDelta],
        [br + rDelta, bc + cDelta],
      ];
    }
    if (ar > br && ar > br) {
      return [
        [ar + rDelta, ar + cDelta],
        [br - rDelta, bc - cDelta],
      ];
    }
    if (ar < br && ac > bc) {
      return [
        [ar - rDelta, ac + cDelta],
        [br + rDelta, bc - cDelta],
      ];
    }
    if (ar > br && ac > bc) {
      return [
        [ar + rDelta, ac + cDelta],
        [br - rDelta, bc - cDelta],
      ];
    }
    throw new Error('Huh?!');
  };

const antiNodes = Object.values(freqToCoords).reduce((acc, coords) => {
  const combos = combinations2(coords);
  const antiNodeCoords = combos
    .flatMap(([a, b]) => {
      const [ar, ac] = stringToCoord(a);
      const [br, bc] = stringToCoord(b);
      const dr = Math.abs(ar - br);
      const dc = Math.abs(ac - bc);
      return getPlacementOps([ar, ac], [br, bc])([dr, dc]);
    })
    .filter(isInRange);
  antiNodeCoords.forEach(c => acc.add(R.toString(c)));
  return acc;
}, new Set<string>());

console.log(antiNodes.size);

// const antiNodesGrid = Array.from(antiNodes).map<[string, string]>(c => [c, '#']);

// const updatedGrid = new Map<string, string>([...grid, ...antiNodesGrid]);

// console.log(gridToString(rMax, cMax, '.', updatedGrid));
