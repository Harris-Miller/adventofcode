/* eslint-disable @typescript-eslint/no-shadow */
/* eslint-disable no-param-reassign */

const puzzleInput = 380621;

const nextInLoop = (current: number, toAdd: number, len: number) => (current + toAdd) % len;

// addToList :: IntMap Int -> Int -> IntMap Int
// addToList xs v =
//   let a = IntMap.fromAscList $ zip [length xs ..] $ map digitToInt (show v)
//    in xs `IntMap.union` a

const addToList = (list: number[], value: number) => {
  const toAdd = String(value)
    .split('')
    .map(x => Number.parseInt(x, 10));
  list.push(...toAdd);
  return list;
};

const part1 = (lengthNeeded: number, list: number[], i1: number, i2: number): number[] => {
  let len = list.length;
  while (lengthNeeded > len) {
    const v1 = list[i1];
    const v2 = list[i2];
    list = addToList(list, v1 + v2);
    len = list.length;
    i1 = nextInLoop(i1, 1 + v1, len);
    i2 = nextInLoop(i2, 1 + v2, len);
  }

  return list;
};

const list = [3, 7];
const i1 = 0;
const i2 = 1;

const lengthNeeded = puzzleInput;

const r1 = part1(lengthNeeded + 10, [...list], i1, i2);
const r1AsStr = r1.slice(lengthNeeded, lengthNeeded + 10).join('');
console.log(r1AsStr);

const part2 = (toFind: string, list: number[]): number => {
  const toFindLen = toFind.length;
  const len = list.length - toFind.length;
  let i = 0;
  let sub: string;
  while (i <= len) {
    sub = list.slice(i, i + toFindLen).join('');
    if (toFind === sub) return i;
    i += 1;
  }

  throw new Error(`Could not find index for subStr ${toFind}`);
};

const r2 = part1(lengthNeeded * 100, [...list], i1, i2);
const r2i = part2(String(puzzleInput), r2);
console.log(r2i);
