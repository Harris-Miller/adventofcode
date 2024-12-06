import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';

const content = (await Bun.file('../inputs/2024/Day5/input.txt').text()).trim();

const [a, b] = content.split('\n\n').map(section => section.split('\n'));

const rules = a.map(line => line.split('|').map(parseInt10)) as [number, number][];
const pages = b.map(line => line.split(',').map(parseInt10));

const inRightOrder = (rule: [number, number], page: number[]) => {
  const [i1, i2] = rule.map(v => page.indexOf(v)) as [number, number];
  if (i1 === -1 || i2 === -1) return true;
  return i1 < i2;
};

const getMiddleValue = <T>(arr: T[]): T => {
  const i = Math.floor(arr.length / 2);
  return arr[i];
};

const [correctOrder, incorrectOrder] = R.partition(page => rules.every(rule => inRightOrder(rule, page)), pages);

const r1 = R.sum(correctOrder.map(getMiddleValue));
console.log(r1);

const updated = incorrectOrder.map(line =>
  R.sort((l, r) => (rules.find(([ll, rr]) => r === rr && l === ll) ? -1 : 1), line),
);

const r2 = R.sum(updated.map(getMiddleValue));
console.log(r2);
