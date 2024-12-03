import * as R from 'ramda';

const content = (await Bun.file('../inputs/2024/Day2/input.txt').text()).trim();

const lines = content.split('\n').map(line => line.split(' ').map(x => Number.parseInt(x, 10)));

// console.log(lines);

// part 1

const areAllIncreasing = (ref: number[]) =>
  R.init(ref).reduce((acc, val, i) => {
    if (!acc) return acc;
    const otherVal = ref[i + 1];
    return otherVal >= val + 1 && otherVal <= val + 3;
  }, true);

const areAllDecreasing = (ref: number[]) =>
  R.init(ref).reduce((acc, val, i) => {
    if (!acc) return acc;
    const otherVal = ref[i + 1];
    return otherVal <= val - 1 && otherVal >= val - 3;
  }, true);

const validateRow = (ref: number[]) => areAllIncreasing(ref) || areAllDecreasing(ref);

const r1 = lines.map(validateRow).filter(R.identity<boolean>).length;
console.log(r1);

// part 2

const tryAgainRemovingOneAtATime = (ref: number[]) =>
  R.range(0, ref.length).reduce((acc, idx) => {
    if (acc) return acc;
    const adjusted = R.remove(idx, 1, ref);
    return areAllIncreasing(adjusted) || areAllDecreasing(adjusted);
  }, false);

const validateRow2 = (ref: number[]) =>
  areAllIncreasing(ref) || areAllDecreasing(ref) || tryAgainRemovingOneAtATime(ref);

const r2 = lines.map(validateRow2).filter(R.identity<boolean>).length;
console.log(r2);
