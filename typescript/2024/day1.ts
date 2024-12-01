import * as R from 'ramda';

const content = (await Bun.file('../inputs/2024/Day1/input.txt').text()).trim();

const rLineRegex = /([0-9]+)\s+([0-9]+)/;

const parse = (str: string): [number, number] => {
  const [, a, b] = str.match(rLineRegex)!;
  return [Number.parseInt(a, 10), Number.parseInt(b, 10)];
};

const asTwoLists = R.flow(content, [
  c => c.split('\n'),
  c => c.map(parse),
  R.transpose<number>,
  c => c.map(x => x.sort()),
]);

const pairs = R.transpose(asTwoLists);

// console.log(pairs);

const r1 = R.sum(pairs.map(([l, r]) => Math.abs(r - l)));

console.log(r1);

const [ll, rr] = asTwoLists;

const sims = ll.map(v => {
  const otherLength = rr.filter(r => r === v).length;
  return v * otherLength;
});

const r2 = R.sum(sims);
console.log(r2);
