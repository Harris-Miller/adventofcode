import { breadthFirstSearch, breadthFirstTraversal } from 'fp-search-algorithms';
import * as R from 'ramda';

type Space = 'Open' | 'Wall';

const fave = 1350;

const calcSpace = ([x, y]: [number, number]): Space => {
  const temp = x * x + 3 * x + 2 * x * y + y + y * y + fave;
  const asString = temp.toString(2).split('');
  return asString.filter(s => s === '1').length % 2 === 0 ? 'Open' : 'Wall';
};

const start: [number, number] = [1, 1];
const end: [number, number] = [31, 39];

const get4Neighbors = ([x, y]: [number, number]): [number, number][] => {
  const a: [number, number][] = [
    [x - 1, y],
    [x, y + 1],
    [x + 1, y],
    [x, y - 1],
  ];

  return a.filter(([l, r]) => l >= 0 && r >= 0);
};

const getNextState = (state: [number, number]): [number, number][] =>
  get4Neighbors(state).filter(n => calcSpace(n) === 'Open');

const r1 = breadthFirstSearch(getNextState, R.equals(end), start);

console.log(r1![1].length - 1);

const visited = new Set<string>();
for (const [value, pathTo] of breadthFirstTraversal(getNextState, start)) {
  visited.add(R.toString(value));
  if (pathTo.length > 50) break;
}

console.log(visited);
console.log(visited.size);
