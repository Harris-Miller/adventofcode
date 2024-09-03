import { aperture, last, sum } from 'ramda';

const content = (await Bun.file('../inputs/2023/Day9/input.txt').text())
  .trim()
  .split('\n')
  .map(row => row.split(' ').map(s => Number.parseInt(s, 10)));

// console.log(content);

const process1 = (history: number[]) => {
  let total = 0;
  let arr = history;

  while (!arr.every(v => v === 0)) {
    total += last(arr)!;
    arr = aperture(2, arr).reduce<number[]>((acc, [l, r]) => {
      acc.push(r - l);
      return acc;
    }, []);
  }

  return total;
};

const result1 = sum(content.map(process1));

console.log(result1);
