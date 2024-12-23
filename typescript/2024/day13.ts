import { depthFirstSearch } from 'fp-search-algorithms';
import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';
import type { Point } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day13/input.txt').text()).trim();

const parseLine = (line: string) => line.split(', ').map(x => parseInt10(R.drop(2, x))) as Point;

type Moves = { buttonA: Point; buttonB: Point; prize: Point };

type Selection = 'A' | 'B' | 'S'; // S == start

type State = [selection: Selection, position: Point];

const parse = (lines: string[]): Moves => ({
  buttonA: parseLine(R.drop(10, lines[0])),
  buttonB: parseLine(R.drop(10, lines[1])),
  prize: parseLine(R.drop(7, lines[2])),
});

const moves = content.split('\n\n').map(c => parse(c.split('\n')));

// console.log(moves);

const createFound = (prize: Point) => (state: State) => R.equals(prize, state[1]);

const createNext =
  ({ buttonA, buttonB, prize }: Moves) =>
  ([, coord]: State) =>
    (
      [
        ['A', buttonA],
        ['B', buttonB],
      ] as State[]
    )
      .map<State>(([btn, [x, y]]) => [btn, [coord[0] + x, coord[1] + y]])
      .filter(([, [x, y]]) => x <= prize[0] && y <= prize[1]);

const r1 = moves.map(move => {
  const found = createFound(move.prize);
  const next = createNext(move);
  const r = depthFirstSearch(next, found, ['S', [0, 0]]);
  if (R.isNil(r)) return 0;

  return R.sum(
    r[1].map(([s]) => {
      switch (s) {
        case 'S':
          return 0;
        case 'A':
          return 3;
        case 'B':
          return 1;
        default:
          throw new Error('should be exhaustive');
      }
    }),
  );
});

console.log(R.sum(r1));

const movesAdjusted = moves.map<Moves>(({ buttonA, buttonB, prize }) => ({
  buttonA,
  buttonB,
  prize: [prize[0] + 10000000000000, prize[1] + 10000000000000],
}));
