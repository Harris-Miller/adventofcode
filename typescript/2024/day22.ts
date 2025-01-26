import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';

const content = (await Bun.file('../inputs/2024/Day22/input.txt').text()).trim().split('\n').map(BigInt);

// console.log(content);

// eslint-disable-next-line no-bitwise
const mixNPrune = (secret: bigint, result: bigint): bigint => (secret ^ result) % BigInt(16777216);

const doTheThing = (secret: bigint): bigint => {
  let r = mixNPrune(secret, secret * BigInt(64));
  r = mixNPrune(r, r / BigInt(32));
  r = mixNPrune(r, r * BigInt(2048));
  return r;
};

const sumBigInt = (arr: bigint[]) => arr.reduce((a, b) => a + b, BigInt(0));

const scanned = content.map(v => R.scan(doTheThing, v, Array(2000).fill(undefined)));
const lasts = scanned.map(a => R.last(a)!);

const part1 = sumBigInt(lasts);

console.log(part1);

const firstDigits = scanned.map(ss => ss.map(s => parseInt10(R.last(s.toString())!)));

console.log(firstDigits[0]);

const doThing2 = (seq: number[]) => {
  const rtn = new Map<string, number>();
  for (let i = 4; i < seq.length; i++) {
    rtn.set(
      [seq[i - 3] - seq[i - 4], seq[i - 2] - seq[i - 3], seq[i - 1] - seq[i - 2], seq[i] - seq[i - 1]].toString(),
      seq[i],
    );
  }
  return rtn;
};

const mapped = firstDigits.map(doThing2);

const seen = new Set<string>();
let bestSeq: string;
let bestVal = 0;

for (const map of mapped) {
  // eslint-disable-next-line @typescript-eslint/no-loop-func
  map.forEach((_, k) => {
    if (seen.has(k)) return;
    seen.add(k);
    const r = R.sum(mapped.map(m => m.get(k) ?? 0));
    if (r > bestVal) {
      bestVal = r;
      bestSeq = k;
    }
  });
}

// @ts-expect-error
console.log(bestSeq, bestVal);
