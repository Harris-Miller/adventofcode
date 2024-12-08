import * as R from 'ramda';

const content = (await Bun.file('../inputs/2024/Day4/input.txt').text())
  .trim()
  .split('\n')
  .map(l => l.split(''));

// vectors is a list of indexes for all possible horizontal, vertical, and diagonals
const vectors: string[] = [];

const ROW_LENGTH = content.length;
const COL_LENGTH = content[0].length;

const rowIndexes = R.range(0, ROW_LENGTH);
const colIndexes = R.range(0, COL_LENGTH);

// rows forward and backward
for (const row of content) {
  vectors.push(row.join(''));
}

// columns forward and backward
for (const i of colIndexes) {
  const col = R.zip(rowIndexes, R.repeat(i, COL_LENGTH))
    .map(([r, c]) => content[r][c])
    .join('');
  vectors.push(col);
}

for (const cStart of R.range(1, COL_LENGTH - 3).toReversed()) {
  let r = 0;
  let c = cStart;
  let str = '';
  while (c < COL_LENGTH) {
    str += content[r][c];
    r += 1;
    c += 1;
  }
  vectors.push(str);
}

for (const rStart of R.range(0, ROW_LENGTH - 3)) {
  let r = rStart;
  let c = 0;
  let str = '';
  while (r < ROW_LENGTH) {
    str += content[r][c];
    r += 1;
    c += 1;
  }
  vectors.push(str);
}

for (const cStart of R.range(3, COL_LENGTH - 1)) {
  let r = 0;
  let c = cStart;
  let str = '';
  while (c >= 0) {
    str += content[r][c];
    r += 1;
    c -= 1;
  }
  vectors.push(str);
}

for (const rStart of R.range(0, ROW_LENGTH - 3)) {
  let r = rStart;
  let c = COL_LENGTH - 1;
  let str = '';
  while (r < ROW_LENGTH) {
    str += content[r][c];
    r += 1;
    c -= 1;
  }
  vectors.push(str);
}

let count = 0;

for (const vec of vectors) {
  // console.log(vec);
  count += (vec.match(/XMAS/g) ?? []).length;
  count += (vec.match(/SAMX/g) ?? []).length;
}

console.log(count);
