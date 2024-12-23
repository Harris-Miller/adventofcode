import { breadthFirstTraversal } from 'fp-search-algorithms';
import * as R from 'ramda';

import { fst, parseInt10 } from '../lib/fp';
import { getNeighbors4, getPoint, gridEntries, parseToGrid } from '../lib/grid';
import type { Point } from '../lib/grid';

const content = (await Bun.file('../inputs/2024/Day10/input.txt').text()).trim();

// console.log(content);

const grid = parseToGrid(parseInt10, content);

const zeros = gridEntries(grid)
  .filter(([, val]) => val === 0)
  .map(fst);

const next = (point: Point) => {
  const valShouldBe = getPoint(point, grid)! + 1;
  return getNeighbors4(point).filter(c => {
    const val = getPoint(c, grid);
    return val === valShouldBe;
  });
};

const result1 = zeros.map(
  z =>
    breadthFirstTraversal(next, z)
      .filter(([, path]) => getPoint(R.last(path)!, grid) === 9)
      .toArray().length,
);

const r1 = R.sum(result1);
console.log(r1);

type Node = { children: Node[]; point: Point; val: number };

const generateTree = (point: Point): Node => {
  const val = getPoint(point, grid)!;
  const nextValShouldBe = val + 1;
  const ns = getNeighbors4(point).filter(p => getPoint(p, grid) === nextValShouldBe);

  return {
    children: ns.map(generateTree),
    point,
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
