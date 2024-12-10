export const swapInPlace = <T>(i1: number, i2: number, arr: T[]): T[] => {
  const v1 = arr[i1];
  const v2 = arr[i2];
  // eslint-disable-next-line no-param-reassign
  arr[i1] = v2;
  // eslint-disable-next-line no-param-reassign
  arr[i2] = v1;
  return arr;
};
