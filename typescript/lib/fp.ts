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
