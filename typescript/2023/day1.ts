import { head, isEmpty, last, sum, tail } from 'ramda';

const content = (await Bun.file('../../inputs/2023/Day1/input.txt').text()).split('\n').filter(Boolean);

// console.log(content);

// part 1
const getFirstAndLastDigit = (str: string) => {
  const ints = str
    .split('')
    .map(x => Number.parseInt(x, 10))
    .filter(x => !Number.isNaN(x));

  return head(ints)! * 10 + last(ints)!;
};

console.log(sum(content.map(getFirstAndLastDigit)));

// part 2
const numberMap = new Map<string, number>([
  ['one', 1],
  ['two', 2],
  ['three', 3],
  ['four', 4],
  ['five', 5],
  ['six', 6],
  ['seven', 7],
  ['eight', 8],
  ['nine', 9],
]);

const collectNumbers = (acc: number[], str: string): number[] => {
  if (isEmpty(str)) return acc;

  const x = head(str)!;
  const maybeDigit = Number.parseInt(x, 10);
  if (!Number.isNaN(maybeDigit)) {
    acc.push(maybeDigit);
  }

  for (const asText of numberMap.keys()) {
    if (str.startsWith(asText)) {
      acc.push(numberMap.get(asText)!);
    }
  }

  return collectNumbers(acc, tail(str));
};

console.log(sum(content.map(str => collectNumbers([], str)).map(x => Number.parseInt(`${head(x)!}${last(x)!}`, 10))));
