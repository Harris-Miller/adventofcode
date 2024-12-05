import * as R from 'ramda';

const content = (await Bun.file('../inputs/2024/Day4/sample.txt').text())
  .trim()
  .split('\n')
  .map(l => l.split(''));

// vectors is a list of indexes for all possible horizontal, vertical, and diagonals
const vectors: [number, number][][] = [];

const ROW_LENGTH = content.length;
const COL_LENGTH = content[0].length;

const rowIndexes = R.range(0, COL_LENGTH);
const rowIndexesR = rowIndexes.toReversed();
const colIndexes = R.range(0, COL_LENGTH);
const colIndexesR = colIndexes.toReversed();

// columns forward and backward
for (const i of rowIndexes) {
  vectors.push(R.zip(R.repeat(i, ROW_LENGTH), colIndexes));
  vectors.push(R.zip(R.repeat(i, ROW_LENGTH), colIndexesR));
}

// rows forward and backward
for (const i of colIndexes) {
  vectors.push(R.zip(R.repeat(i, COL_LENGTH), rowIndexes));
  vectors.push(R.zip(R.repeat(i, COL_LENGTH), rowIndexesR));
}

// diagonals
for (const i of rowIndexes) {
  for (const j of colIndexes) {
    // lol fuck this, ain't nobody got time to calculate this bullshit
  }
}
