//
// Let's make our own special reduce
//

// define type for our special ReducerFn
// same as a normal one, expect you can also return `Reduced<u>`
export type ReducerFn<U, T> = (acc: U, value: T) => Reduced<U> | U;

// special symbol and type
export const reducedSymbol = Symbol('reduced');
export type Reduced<T> = { [reducedSymbol]: T };

// function to "lift" `T` into `Reduced<T>`
export const reduced = <T>(value: T): Reduced<T> => ({
  [reducedSymbol]: value,
});

// determine if `T | Reduced<T>` is `Reduced<T>
export const isReduced = <T>(value: Reduced<T> | T): value is Reduced<T> =>
  Object.hasOwn(value as object, reducedSymbol);

// extract `T` from `Reduced<T>`
export const fromReduced = <T>(value: Reduced<T>) => value[reducedSymbol];

// and finally our special reducer
// this one takes our special `ReducerFn`
// as well as allowing for not just an array, but any `Iterable`
export const reduce = <T, U>(reduceFn: ReducerFn<U, T>, init: U, iter: Iterable<T>) => {
  let acc: Reduced<U> | U = init;
  // we use a simple for-of loop
  for (const val of iter) {
    acc = reduceFn(acc, val);
    // special handler for `reduced`
    // which just bails if any val comes back as `Reduced`
    if (isReduced(acc)) {
      acc = fromReduced(acc);
      break;
    }
  }

  return acc;
};

// async version !!
export const reduceAsync = async <T, U>(reduceFn: ReducerFn<U, T>, init: U, iter: AsyncIterable<T>) => {
  let acc: Reduced<U> | U = init;
  // we use a simple for-of loop
  for await (const val of iter) {
    acc = reduceFn(acc, val);
    // special handler for `reduced`
    // which just bails if any val comes back as `Reduced`
    if (isReduced(acc)) {
      acc = fromReduced(acc);
      break;
    }
  }

  return acc;
};
