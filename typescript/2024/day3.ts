import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';

const content = (await Bun.file('../inputs/2024/Day3/input.txt').text()).trim().split('\n').join('');

const rMul = /mul\(\d{1,4},\d{1,4}\)/g;

const muls = content.match(rMul)!;

const r1 = muls.map(
  R.pipe(
    (s: string) => R.drop(4, s),
    s => R.init(s),
    R.split(','),
    R.map(parseInt10),
    R.product,
  ),
);

console.log(R.sum(r1));
