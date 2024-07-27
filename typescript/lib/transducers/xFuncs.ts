import type { ReducerFn } from './reduce';
import { reduced } from './reduce';
import type { Transformer } from './transduce';

//
// there functions are all called `Transducers`
// `Transducers` return `Transformers`
//

// [].filter((val: number) => val & 1);

export const xMap =
  <A, B>(mapFn: (val: A) => B): Transformer<A, B> =>
  next =>
  (acc: unknown, val: A) =>
    next(acc, mapFn(val));

export const xFilter: {
  <A, AN extends A>(pred: (val: A) => val is AN): Transformer<A, AN>;
  <A>(pred: (val: A) => boolean): Transformer<A, A>;
} =
  <A>(pred: (val: A) => boolean): Transformer<A, A> =>
  next =>
  (acc: unknown, val: A) =>
    pred(val) ? next(acc, val) : acc;

export const xFlatMap =
  <A, B>(mapFn: (val: A) => B[]): Transformer<A, B> =>
  next =>
  (acc: unknown, val: A) =>
    mapFn(val).reduce(next, acc);

export const xFlatten =
  <A>(next: ReducerFn<unknown, A>): ReducerFn<unknown, A[]> =>
  (acc: unknown, val: A[]) =>
    val.reduce(next, acc);

//
// cool extra's we can't do with simple reduce
//

/**
 * Take first n entries
 */
export const xTake = <A>(n: number): Transformer<A, A> => {
  if (n <= 0) throw new Error('xTake param but be greater than 0');

  return next => {
    let i = 0;
    return (acc: unknown, val: A) => {
      i += 1;
      if (n === 0) return acc;
      const ret = next(acc, val);
      return i >= n ? reduced(ret) : ret;
    };
  };
};

/**
 * Drop first n entries
 */
export const xDrop = <A>(n: number): Transformer<A, A> => {
  if (n <= 0) throw new Error('xDrop param but be greater than 0');

  return next => (acc: unknown, val: A) => {
    let i = n;
    if (i > 0) {
      i -= 1;
      return acc;
    }
    return next(acc, val);
  };
};

/**
 * Drop duplicates
 */
export const xUniqBy =
  <A, T>(uniqByFn: (val: A) => T): Transformer<A, A> =>
  next => {
    const seen = new Set<T>();
    return (acc: unknown, val: A) => {
      const t = uniqByFn(val);
      if (seen.has(t)) return acc;
      seen.add(t);
      return next(acc, val);
    };
  };

/**
 * Perform a side-effect, returning input
 */
export const xTap =
  <A>(fn: (value: A) => void): Transformer<A, A> =>
  next =>
  (acc: unknown, value: A) => {
    fn(value);
    return next(acc, value);
  };
