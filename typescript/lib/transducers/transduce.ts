import { fromReduced, isReduced, reduce, reduceAsync } from './reduce';
import type { Reduced, ReducerFn } from './reduce';

export type Transformer<A, B> = (nextReducer: ReducerFn<unknown, B>) => ReducerFn<unknown, A>;

export const transduce = <U, A, B>(
  xf: Transformer<A, B>,
  endReduceFn: ReducerFn<U, B>,
  initialAcc: U,
  iter: Iterable<A>,
): U => {
  const reducer = xf(endReduceFn as ReducerFn<unknown, B>) as ReducerFn<U, A>;

  return reduce(reducer, initialAcc, iter);
};

export const transduceAsync = async <U, A, B>(
  xf: Transformer<A, B>,
  endReduceFn: ReducerFn<U, B>,
  initialAcc: U,
  iter: AsyncIterable<A>,
): Promise<U> => {
  const reducer = xf(endReduceFn as ReducerFn<unknown, B>) as ReducerFn<U, A>;
  return reduceAsync(reducer, initialAcc, iter);
};

const collectIntoArray = <T>(acc: T[], value: T) => [...acc, value];

export const into = <A, T>(arr: T[], transducer: Transformer<A, T>, list: A[]) =>
  transduce<T[], A, T>(transducer, collectIntoArray, arr, list);

export const intoEmpty = <A, T>(transducer: Transformer<A, T>, list: A[]) => into<A, T>([], transducer, list);
