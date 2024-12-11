import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';

const content = (await Bun.file('../inputs/2024/Day11/input.txt').text()).trim();

// console.log(content);

const getNextStones = (stone: number): number[] => {
  if (stone === 0) return [1];
  const str = stone.toString();
  // eslint-disable-next-line no-bitwise
  if ((str.length & 1) === 0) return R.splitAt(str.length / 2, str).map(parseInt10);
  return [stone * 2024];
};

const process = (input: number[], times: number): number[] => {
  let result = input;
  for (let i = 0; i < times; ++i) {
    result = result.flatMap(getNextStones);
  }
  return result;
};

const start = content.split(' ').map(parseInt10);

const r1 = process(start, 25);
console.log(r1.length);

// memoize the length for `n` and number of blinks
const memo = new Map<string, number>();

const process2 = (stone: number, remaining: number): number => {
  const key = `${stone}-${remaining}`;
  const fromMemo = memo.get(key);
  if (fromMemo != null) return fromMemo;

  if (remaining === 1) {
    return getNextStones(stone).length;
  }

  const nextStones = getNextStones(stone);

  const count = R.sum(nextStones.map(nextStone => process2(nextStone, remaining - 1)));

  memo.set(key, count);

  return count;
};

const r2 = R.sum(start.map(stone => process2(stone, 75)));
console.log(r2);
