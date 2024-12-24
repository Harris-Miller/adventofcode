import { dijkstra, generateTreeBreadthFirst, HashMap, HashSet, isEqual } from 'fp-search-algorithms';
import * as R from 'ramda';

import { divvy, parseInt10 } from '../lib/fp';
import { getDirection, getNeighbors4 } from '../lib/grid';
import type { Direction, Point } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day21/sample.txt').text()).trim();

// console.log(content);

type Arrow = '^' | '<' | '>' | 'A' | 'v';

const sequences = content.split('\n').map(line => line.split(''));

const numpadMap = new HashMap<string, Point>([
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

const validNumpadPoints = new HashSet<Point>(numpadMap.values());

const arrowpadMap = new HashMap<string, Point>([
  ['^', [0, 1]],
  ['A', [0, 2]],
  ['<', [1, 0]],
  ['v', [1, 1]],
  ['>', [1, 2]],
]);

const validArrowpadPoints = new HashSet<Point>(arrowpadMap.values());

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
    const [, pathTo] = dijkstra(next, R.always(1), found, this.point)!;
    this.point = R.last(pathTo)!;
    const sequence = R.init(divvy(2, 1, pathTo)).map(([from, to]) => directionToArrow(getDirection(from, to)));
    return [...sequence, 'A'];
  }
}

const numpadNext = (point: Point) => getNeighbors4(point).filter(p => validArrowpadPoints.has(p));

const calcNumpadMoves = ([from, to]: [from: Point, to: Point]) => {
  let shortest = Infinity;
  let collection: string[] = [];
  for (const [visit, pathFromStart] of generateTreeBreadthFirst(numpadNext, from)) {
    if (isEqual(visit, to) && pathFromStart.length - 1 <= shortest) {
      const arrows = R.init(divvy(2, 1, pathFromStart))
        .map(([l, r]) => directionToArrow(getDirection(l, r)))
        .join('');
      if (collection.length === 0) {
        collection.push(arrows);
      } else if (arrows.length > collection[0].length) {
        continue;
      } else if (arrows.length < collection[0].length) {
        shortest = arrows.length;
        collection = [arrows];
      } else {
        collection.push(arrows);
      }
    }
  }
  return shortest;
};

const r1 = R.take(1, sequences).map((sequence: string[]) => {
  const buttons = ['A', ...sequence].map(str => numpadMap.get(str)!);
  console.log(buttons);
  const pairs = R.init(divvy(2, 1, buttons) as [Point, Point][]);
  console.log(pairs);
  const temp = R.init(divvy(2, 1, buttons) as [Point, Point][]).map(([from, to]) => {
    const a = calcNumpadMoves([from, to]);
    return a;
  });
  return temp;
  // R.init(divvy(2, 1, temp) as [Point, Point][]).map(([l, r]) => getDirection(l, r));
});

console.log(r1[0]);

// class Arrowpad {
//   point: Point;
//   constructor(start: Point) {
//     this.point = start;
//   }

//   move(destination: Point): Arrow[] {
//     const next = (point: Point) => getNeighbors4(point).filter(p => validArrowpadPoints.has(p));
//     const found = (point: Point) => R.equals(point, destination);
//     const [, pathTo] = dijkstra(next, R.always(1), found, this.point)!;
//     this.point = R.last(pathTo)!;
//     const sequence = R.init(divvy(2, 1, pathTo)).map(([from, to]) => directionToArrow(getDirection(from, to)));
//     return [...sequence, 'A'];
//   }
// }

// const calcComplexity = (sequence: string[]): number => {
//   const numpad = new Numpad(numpadMap.get('A')!);
//   const arrow1 = new Arrowpad(arrowpadMap.get('A')!);
//   const arrow2 = new Arrowpad(arrowpadMap.get('A')!);

//   return sequence
//     .flatMap(num => {
//       const arrowSeq1 = numpad.move(numpadMap.get(num)!);
//       // console.log(arrowSeq1.join(''));
//       const arrowSeq2 = arrowSeq1.flatMap(a => arrow1.move(arrowpadMap.get(a)!));
//       // console.log(arrowSeq2.join(''));
//       const arrowSeq3 = arrowSeq2.flatMap(a => arrow2.move(arrowpadMap.get(a)!));
//       // console.log(arrowSeq3.join(''));
//       return arrowSeq3;
//     })
//     .join('').length;
// };

// const getNum = (sequence: string[]) => parseInt10(R.init(sequence).join(''));

// const pairs = sequences.map(seq => [calcComplexity(seq), getNum(seq)] as const);

// console.log(pairs);

// const r1 = R.sum(pairs.map(([l, r]) => l * r));

// console.log(r1);
