import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';

const content = (await Bun.file('../inputs/2024/Day11/input.txt').text()).trim();

// console.log(content);

const process = (input: number[], times: number): number[] => {
  let result = input;
  for (let i = 0; i < times; ++i) {
    result = result.flatMap(num => {
      if (num === 0) return [1];
      const str = num.toString();
      // eslint-disable-next-line no-bitwise
      if ((str.length & 1) === 0) return R.splitAt(str.length / 2, str).map(parseInt10);
      return [num * 2024];
    });
  }
  return result;
};

const start = content.split(' ').map(parseInt10);

const r1 = process(start, 25);
console.log(r1.length);

const r2 = process(start, 75);
console.log(r2.length);
