import { difference, init, isNotNil } from 'ramda';

import { combinations2 } from '../lib/fp';
import { aStar } from '../lib/searchAlgorithms';

type Floor = 'f1' | 'f2' | 'f3' | 'f4';
type State = {
  e: Floor;
  f1: string[];
  f2: string[];
  f3: string[];
  f4: string[];
};

// The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
// The second floor contains a hydrogen generator.
// The third floor contains a lithium generator.
// The fourth floor contains nothing relevant.
const sampleState: State = {
  e: 'f1',
  f1: ['hydrogenM', 'lithiumM'],
  f2: ['hydrogenG'],
  f3: ['lithiumG'],
  f4: [],
};

// The first floor contains a promethium generator and a promethium-compatible microchip.
// The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
// The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
// The fourth floor contains nothing relevant.
//
// Wrong, but accepted answers: Part1 = 33, Part2 = 57
// See more here: https://www.reddit.com/r/adventofcode/comments/5hqxzq/2016_day_11_can_we_get_a_list_of_inputssolutions/db27t2c/
const initialState: State = {
  e: 'f1',
  f1: ['promethiumG', 'promethiumM'],
  f2: ['cobaltG', 'curiumG', 'rutheniumG', 'plutoniumG'],
  f3: ['cobaltM', 'curiumM', 'rutheniumM', 'plutoniumM'],
  f4: [],
};

const nextFloorOptions = (e: Floor): Floor[] => {
  switch (e) {
    case 'f1':
      return ['f2'];
    case 'f2':
      return ['f1', 'f3'];
    case 'f3':
      return ['f2', 'f4'];
    case 'f4':
      return ['f3'];
    default:
      throw new Error('non-exhausted option');
  }
};

const calculateMoves = (ee: Floor[], xs: string[][]): [Floor, string[]][] =>
  xs.flatMap(x => ee.map<[Floor, string[]]>(e => [e, x]));

const whatCanGetMoved = (cs: string[]): string[][] => {
  const xs1 = cs.map(x => [x]);
  const xs2 = combinations2(cs);
  return [...xs1, ...xs2];
};

const doMove =
  (state: State) =>
  ([e, xs]: [Floor, string[]]) => {
    const newFloor = [...state[e], ...xs].sort();

    // get all microchips
    const ms = newFloor.filter(m => m.endsWith('M')).map(s => init(s));
    const gs = newFloor.filter(m => m.endsWith('G')).map(s => init(s));

    // if there is at least 1 generator and that generator is not for any of the microchips
    // return undefined for illegal state
    if (gs.length > 0 && ms.some(m => !gs.includes(m))) return undefined;

    // return change in state
    return {
      e,
      [state.e]: difference(state[state.e], xs).sort(),
      [e]: newFloor,
    };
  };

const getNextStates = (state: State): State[] => {
  // can only move a floor at a time
  const ee = nextFloorOptions(state.e);
  // figure out what _can_ be moved together or separately
  const xs = whatCanGetMoved(state[state.e]);

  const options = calculateMoves(ee, xs);

  const nextPartials = options.map(doMove(state)).filter(isNotNil);

  return nextPartials.map(np => ({ ...state, ...np }));
};

const estimateRemainingCost = (state: State) => state.f1.length * 3 + state.f2.length * 2 + state.f3.length;

const determineIfFound = (state: State) =>
  state.e === 'f4' && state.f1.length === 0 && state.f2.length === 0 && state.f3.length === 0;

export const part1 = aStar(getNextStates, () => 1, estimateRemainingCost, determineIfFound, sampleState);

console.log(part1?.[0]);
console.log(part1?.[1]);
