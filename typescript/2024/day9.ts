import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';
import { swapInPlace } from '../lib/mut';

const content = (await Bun.file('../inputs/2024/Day9/input.txt').text()).trim();

const blocks = content.split('').flatMap((v, i) => {
  const thing = i % 2 === 1 ? '.' : i / 2;
  return R.repeat(thing, parseInt10(v));
});

// console.log(blocks.join(''));

const compacted = R.clone(blocks);
let periodIndex = compacted.findIndex(v => v === '.');
let valIndex = compacted.findLastIndex(v => v !== '.');

while (periodIndex < valIndex) {
  swapInPlace(periodIndex, valIndex, compacted);
  periodIndex = compacted.findIndex(v => v === '.');
  valIndex = compacted.findLastIndex(v => v !== '.');
}

// console.log(compacted.join(''));

const r1 = R.sum(compacted.filter((v): v is number => v !== '.').map(R.multiply));

console.log(r1);

const compacted2 = R.clone(blocks).join('').split('');

// console.log(compacted2.join(''));

const indexesToCheck = R.range(0, Math.ceil(content.length / 2)).reverse();

const findSequences = (str: string, seq: string): [startIndex: number, endIndex: number] => {
  const startIndex = str.lastIndexOf(seq);
  const len = seq.length;
  let nextIndex: number;
  let index = startIndex;
  let back: string;
  // eslint-disable-next-line no-constant-condition
  while (true) {
    nextIndex = index - len;
    back = str.substring(nextIndex, nextIndex + len);
    if (back !== seq) return [index, startIndex + len - 1];
    index = nextIndex;
  }
};

const findEmptySpace = (str: string, size: number, max: number): number | undefined => {
  const index = str.indexOf(R.repeat('.', size).join(''));
  if (index < 0 || index > max) return undefined;
  return index;
};

// const asStr = compacted2.join('');
// console.log(asStr);
// // const first = indexesToCheck[0].toString();
// const first = '9';
// console.log(first);

// const [f, l] = findSequences(compacted2.join(''), first);

// const spaceIndexStart = findEmptySpace(asStr, l - f, l);

// console.log([f, l], spaceIndexStart);

for (const num of indexesToCheck) {
  const asStr = compacted2.join('');
  const [left, right] = findSequences(asStr, num.toString());
  // console.log(left, right);
  const size = right - left + 1;
  // console.log(size);
  const emptyBlockIndexStart = findEmptySpace(asStr, size, left);
  // console.log(emptyBlockIndexStart);
  if (R.isNotNil(emptyBlockIndexStart)) {
    const emptyRange = R.range(emptyBlockIndexStart, emptyBlockIndexStart + size);
    const numRange = R.range(left, right + 1);
    const indexesToSwap = R.zip(emptyRange, numRange);
    // console.log(indexesToSwap);
    indexesToSwap.forEach(([i1, i2]) => {
      swapInPlace(i1, i2, compacted2);
    });
  }
}

// console.log(compacted2.join(''));

const r2 = R.sum(
  compacted2
    .join('')
    .split('')
    .map((v, i) => {
      if (v === '.') return 0;
      return parseInt10(v) * i;
    }),
);

console.log(r2);
