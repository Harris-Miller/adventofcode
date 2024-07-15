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
