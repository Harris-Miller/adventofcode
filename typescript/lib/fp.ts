import * as R from 'ramda';

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

export const reduce1: {
  <T>(fn: (acc: T, value: T) => T): (list: T[]) => T;
  <T>(fn: (acc: T, value: T) => T, list: T[]): T;
} = R.curryN(2, <T>(fn: (acc: T, value: T) => T, list: T[]): T => {
  const [h, ...t] = list;
  return t.reduce(fn, h);
});

export const reduceRight1: {
  <T>(fn: (acc: T, value: T) => T): (list: T[]) => T;
  <T>(fn: (acc: T, value: T) => T, list: T[]): T;
} = R.curryN(2, <T>(fn: (acc: T, value: T) => T, list: T[]): T => {
  const init = R.init(list);
  const l = R.last(list)!;
  return init.reduceRight(fn, l);
});

export const parseInt10 = (str: string) => Number.parseInt(str, 10);
