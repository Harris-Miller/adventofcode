import * as R from 'ramda';

/**
 * Array of tuples into separate arrays
 * ```typescript
 * unzip([[1, 2], [3, 4], [5, 6]])
 * // => [[1, 3, 5], [2, 4, 6]]
 * ```
 */
export const unzip = <A, B>(tupleArray: [A, B][]): [A[], B[]] => {
  const len = tupleArray.length;
  let i = 0;
  const as = Array<A>(len);
  const bs = Array<B>(len);

  while (i < len) {
    const [a, b] = tupleArray[i];
    as[i] = a;
    bs[i] = b;
    i += 1;
  }

  return [as, bs];
};

/**
 * Generate all combinations of values
 * ```typescript
 * const combos = combinations2([1, 2, 3, 4]);
 * // [ [ 1, 2 ], [ 1, 3 ], [ 1, 4 ], [ 2, 3 ], [ 2, 4 ], [ 3, 4 ] ]
 * ```
 */
export const combinations2 = <T>(list: T[]): [T, T][] => {
  const len = list.length;
  const result: [T, T][] = [];
  for (let i = 0; i < len - 1; i += 1) {
    for (let j = i + 1; j < len; j += 1) {
      result.push([list[i], list[j]]);
    }
  }
  return result;
};

/**
 * reduce with first value in array serving as initialValue
 */
export const reduce1: {
  <T>(fn: (acc: T, value: T) => T): (list: T[]) => T;
  <T>(fn: (acc: T, value: T) => T, list: T[]): T;
} = R.curryN(2, <T>(fn: (acc: T, value: T) => T, list: T[]): T => {
  const [h, ...t] = list;
  return t.reduce(fn, h);
});

/**
 * reduceRight with first value in array serving as initialValue
 */
export const reduceRight1: {
  <T>(fn: (acc: T, value: T) => T): (list: T[]) => T;
  <T>(fn: (acc: T, value: T) => T, list: T[]): T;
} = R.curryN(2, <T>(fn: (acc: T, value: T) => T, list: T[]): T => {
  const init = R.init(list);
  const l = R.last(list)!;
  return init.reduceRight(fn, l);
});

/* parseInt at radix: 10 */
export const parseInt10 = (str: string) => Number.parseInt(str, 10);

/**
 * Chunk an array into sub arrays of length n
 * The last piece will be shorter if num does not evenly divide the length of the array.
 */
export const chunksOf = <T>(n: number, arr: T[]): T[][] => {
  const len = arr.length;
  // start with one so modulo logic works rest of the way
  let temp: T[] = [arr[0]];
  const output: T[][] = [];
  for (let i = 1; i < len; i += 1) {
    if (i % n === 0) {
      output.push(temp);
      temp = [];
    }
    temp.push(arr[i]);
  }
  if (temp.length) {
    output.push(temp);
  }
  return output;
};
