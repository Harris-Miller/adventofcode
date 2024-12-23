import { aperture, head, last, sum } from 'ramda';

const content = (await Bun.file('../inputs/2023/Day9/input.txt').text())
  .trim()
  .split('\n')
  .map(row => row.split(' ').map(s => Number.parseInt(s, 10)));

export const process1 = (history: number[]) => {
  const acc: number[] = [];
  let arr = history;

  while (!arr.every(v => v === 0)) {
    acc.push(last(arr)!);
    arr = aperture(2, arr).map(([l, r]) => r - l);
  }

  return sum(acc);
};

const result1 = sum(content.map(process1));

console.log(result1);

export const process2 = (history: number[]) => {
  const acc: number[] = [];
  let arr = history;

  while (!arr.every(v => v === 0)) {
    acc.push(head(arr)!);
    arr = aperture(2, arr).map(([l, r]) => r - l);
  }

  return acc.reduceRight((l, r) => r - l);
};

const result2 = sum(content.map(process2));
console.log(result2);
