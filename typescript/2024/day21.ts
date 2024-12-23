/* eslint-disable max-classes-per-file */
import { breadthFirstSearch, Dict, DSet } from 'fp-search-algorithms';
import * as R from 'ramda';

import { divvy } from '../lib/fp';
import { getDirection, getNeighbors4 } from '../lib/grid';
import type { Direction, Point } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day21/input.txt').text()).trim();

// console.log(content);

type Arrow = '^' | '<' | '>' | 'A' | 'v';

const sequences = content.split('\n').map(line => line.split(''));

const numpadDict = new Dict<string, Point>([
  ['7', [0, 0]],
  ['8', [0, 1]],
  ['9', [0, 2]],
  ['4', [1, 0]],
  ['5', [1, 1]],
  ['6', [1, 2]],
  ['1', [2, 0]],
  ['2', [2, 1]],
  ['3', [2, 2]],
  ['0', [3, 1]],
  ['A', [3, 2]],
]);

const validNumpadPoints = new DSet<Point>(numpadDict.values());

const arrowpadDict = new Dict<string, Point>([
  ['^', [0, 1]],
  ['A', [0, 2]],
  ['<', [1, 0]],
  ['v', [1, 1]],
  ['>', [1, 2]],
]);

const validArrowpadPoints = new DSet<Point>(arrowpadDict.values());

const directionToArrow = (dir: Direction): Arrow => {
  switch (dir) {
    case 'up':
      return '^';
    case 'right':
      return '>';
    case 'down':
      return 'v';
    case 'left':
      return '<';
    default:
      throw new Error('directionToArrow non-exhaustive');
  }
};

class Numpad {
  point: Point;
  constructor(start: Point) {
    this.point = start;
  }

  move(destination: Point): Arrow[] {
    const next = (point: Point) => getNeighbors4(point).filter(p => validNumpadPoints.has(p));
    const found = (point: Point) => R.equals(point, destination);
    const [nextPoint, pathTo] = breadthFirstSearch(next, found, this.point)!;
    console.log(nextPoint, pathTo);
    this.point = nextPoint;
    const sequence = R.init(divvy(2, 1, pathTo)).map(([from, to]) => directionToArrow(getDirection(from, to)));
    return [...sequence, 'A'];
  }
}

class Arrowpad {
  point: Point;
  constructor(start: Point) {
    this.point = start;
  }

  move(destination: Point): Arrow[] {
    const next = (point: Point) => getNeighbors4(point).filter(p => validArrowpadPoints.has(p));
    const found = (point: Point) => R.equals(point, destination);
    const [nextPoint, pathTo] = breadthFirstSearch(next, found, this.point)!;
    this.point = nextPoint;
    const sequence = R.init(divvy(2, 1, pathTo)).map(([from, to]) => directionToArrow(getDirection(from, to)));
    return [...sequence, 'A'];
  }
}

const numpad = new Numpad(numpadDict.get('A')!);
const arrow1 = new Arrowpad(arrowpadDict.get('A')!);
const arrow2 = new Arrowpad(arrowpadDict.get('A')!);
// const arrow3 = new Arrowpad(arrowpadDict.get('A')!);

const determineInputSequence = (sequence: string[]) => {
  console.log(sequence);
  return sequence
    .flatMap(num => {
      const arrowSeq1 = numpad.move(numpadDict.get(num)!);
      const arrowSeq2 = arrowSeq1.flatMap(a => arrow1.move(arrowpadDict.get(a)!));
      const arrowSeq3 = arrowSeq2.flatMap(a => arrow2.move(arrowpadDict.get(a)!));
      return arrowSeq3;
      // return arrowSeq3.flatMap(a => arrow3.move(arrowpadDict.get(a)!));
    })
    .join('');
};

const r1 = R.take(1, sequences).map(determineInputSequence);

console.log(r1);
