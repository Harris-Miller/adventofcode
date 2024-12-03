import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';

const content = (await Bun.file('../inputs/2024/Day3/input.txt').text()).trim().split('\n').join('');

const rMul = /mul\(\d{1,4},\d{1,4}\)/g;

const parsed = content.match(rMul)!;

const doMul = R.pipe(
  (s: string) => R.drop(4, s),
  s => R.init(s),
  R.split(','),
  R.map(parseInt10),
  R.product,
);

const r1 = R.sum(parsed.map(doMul));

console.log(r1);

const rMul2 = /(mul\(\d{1,4},\d{1,4}\)|do\(\)|don't\(\))/g;

const parsed2 = content.match(rMul2)!;

let isOn = true;
let r2 = 0;

for (const m of parsed2) {
  if (m.startsWith("don't")) {
    isOn = false;
  } else if (m.startsWith('do')) {
    isOn = true;
  } else if (isOn) {
    r2 += doMul(m);
  }
}

console.log(r2);
