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

/* parseInt at radix: 10 */
export const parseInt10 = (str: string) => Number.parseInt(str, 10);

/**
 * divvy by a num and offset
 * The last piece will be shorter if num does not evenly divide the length of the array.
 * ```typescript
 * divvy(2, 1, [1, 2, 3, 4])
 * // => [[1, 2], [2, 3], [3, 4], [4]]
 *
 * divvy(2, 2, [1, 2, 3, 4])
 * // => [[1, 2], [3, 4]]
 *
 * divvy(2, 2, [1, 2, 3, 4, 5])
 * // => [[1, 2], [3, 4], [5]]
 * ```
 */
export const divvy = <T>(num: number, offset: number, arr: T[]): T[][] => {
  let i = 0;
  const len = arr.length;
  const result: T[][] = [];
  while (i < len) {
    result.push(arr.slice(i, i + num));
    i += offset;
  }
  return result;
};

/**
 * Chunk an array into sub arrays of length n
 * The last piece will be shorter if num does not evenly divide the length of the array.
 */
export const chunksOf = <T>(n: number, arr: T[]): T[][] => divvy(n, n, arr);

export const fst = <T>(tuple: [T, unknown, unknown] | [T, unknown]) => tuple[0];
export const snd = <T>(tuple: [T, unknown, unknown] | [T, unknown]) => tuple[1];
