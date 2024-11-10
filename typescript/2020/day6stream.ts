// typescript has not added typings yet for the new Set methods
// but both Node and Bun have implemented them, ad-hoc type the intersection method so we can use it in this file
import { compose } from 'ramda';

import fs from 'fs';
import readline from 'readline';

import { transduceAsync, xMap } from '../lib/transducers';
import type { Transformer } from '../lib/transducers';

// declare global {
//   interface Set<T> {
//     intersection(other: Set<T>): Set<T>;
//   }
// }

// the readline AsyncIterator seems to be auto trimming the file and ignores the final empty line
// the algorithm expects that final emptyString to be there
// so I'm wrapping it in an async generator to produce that final emptyString
const createReadline = async function* (path: fs.PathLike) {
  const fileStream = fs.createReadStream(path);

  const rl = readline.createInterface({
    crlfDelay: Infinity,
    input: fileStream,
  });

  yield* rl;
  yield '';
};

const xCollectByEmptyLine: Transformer<string, string[]> = next => {
  let collection: string[] = [];
  return (acc: unknown, val: string) => {
    if (val.trim() === '') {
      const group = collection;
      collection = [];
      return next(acc, group);
    }

    collection.push(val);
    return acc;
  };
};

const processGroupCount1 = (group: string[]) => {
  const combined = group.reduce((acc, x) => acc + x, '');
  const sepByChar = combined.split('');
  const unique = new Set(sepByChar);
  const count = unique.size;
  return count;
};

const part1 = await transduceAsync(
  compose(xCollectByEmptyLine, xMap(processGroupCount1)),
  (acc, x) => acc + x, // sum
  0,
  createReadline('../inputs/2020/Day6/input.txt'),
);

console.log(part1);

const processGroupCount2 = (group: string[]) => {
  const uniqByRow = group.map(x => new Set(x.split('')));
  const [first, ...rest] = uniqByRow;
  const allInCommon = rest.reduce((acc, x) => acc.intersection(x), first);
  const count = allInCommon.size;
  return count;
};

const part2 = await transduceAsync(
  compose(xCollectByEmptyLine, xMap(processGroupCount2)),
  (acc, x) => acc + x, // sum
  0,
  createReadline('../inputs/2020/Day6/input.txt'),
);

console.log(part2);
