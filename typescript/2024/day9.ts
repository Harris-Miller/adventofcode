import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';

const content = (await Bun.file('../inputs/2024/Day9/input.txt').text()).trim();

// console.log(content);

const blocks = content.split('').flatMap((v, i) => {
  const thing = i % 2 === 1 ? '.' : `${i / 2}`;
  return R.repeat(thing, parseInt10(v));
});

// console.log(blocks.join(''));

let compacted = blocks;
let periodIndex = compacted.findIndex(v => v === '.');
let valIndex = compacted.findLastIndex(v => v !== '.');

while (periodIndex < valIndex) {
  compacted = R.swap(periodIndex, valIndex, compacted);
  periodIndex = compacted.findIndex(v => v === '.');
  valIndex = compacted.findLastIndex(v => v !== '.');
}

// console.log(compacted.join(''));

const r1 = R.sum(
  compacted
    .filter(v => v !== '.')
    .map(parseInt10)
    .map(R.multiply),
);

console.log(r1);
