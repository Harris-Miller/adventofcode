import { breadthFirstTraversal } from 'fp-search-algorithms';
import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';
import { collectGrid, getNeighbors4 } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day10/input.txt').text()).trim();

// console.log(content);

const [, grid] = collectGrid(R.T, parseInt10, content);

const zeros = grid
  .entries()
  .filter(([, val]) => val === 0)
  .map(([cStr]) => cStr)
  .toArray();

const next = (cStr: string) => {
  const valShouldBe = grid.get(cStr)! + 1;
  return getNeighbors4(cStr)
    .filter(c => {
      const val = grid.get(R.toString(c));
      return val === valShouldBe;
    })
    .map(R.toString);
};

const result1 = zeros.map(
  z =>
    breadthFirstTraversal(next, z)
      .filter(([, path]) => grid.get(R.last(path)!) === 9)
      .toArray().length,
);

const r1 = R.sum(result1);
console.log(r1);

type Node = { children: Node[]; coord: string; val: number };

const generateTree = (cStr: string): Node => {
  const val = grid.get(cStr)!;
  const nextValShouldBe = val + 1;
  const ns = getNeighbors4(cStr)
    .filter(c => grid.get(R.toString(c)) === nextValShouldBe)
    .map(R.toString);

  return {
    children: ns.map(generateTree),
    coord: cStr,
    val,
  };
};

const forest = zeros.map(generateTree);

/** simple tree traversal that doesn't care about multiple visits */
const traverse = function* (node: Node): Generator<number> {
  yield node.val;
  for (const child of node.children) {
    yield* traverse(child);
  }
};

const result2 = forest.flatMap(zNode => {
  return traverse(zNode)
    .filter(n => n === 9)
    .toArray();
});

console.log(result2.length);
