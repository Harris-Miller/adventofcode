/* eslint-disable line-comment-position */

const content = await Bun.file('../inputs/2020/Day6/input.txt').text();

const groups = content
  .trim() // remove EOF line-break
  .split('\n\n') // sections are split by empty lines '\n\n'
  .map(x => x.split('\n')); // and each section is on it's own line

const processGroupCount1 = (group: string[]) => {
  const combined = group.reduce((acc, x) => acc + x, '');
  const sepByChar = combined.split('');
  const unique = new Set(sepByChar);
  const count = unique.size;
  return count;
};

const processGroups1 = (sections: string[][]) => {
  const groupCounts = sections.map(processGroupCount1);
  const total = groupCounts.reduce((acc, x) => acc + x, 0); // sum
  return total;
};

console.log(processGroups1(groups));

const processGroupCount2 = (group: string[]) => {
  const uniqByRow = group.map(x => new Set(x.split('')));
  const [first, ...rest] = uniqByRow;
  const allInCommon = rest.reduce((acc, x) => acc.intersection(x), first);
  const count = allInCommon.size;
  return count;
};

const processGroups2 = (sections: string[][]) => {
  const groupCounts = sections.map(processGroupCount2);
  const total = groupCounts.reduce((acc, x) => acc + x, 0); // sum
  return total;
};

console.log(processGroups2(groups));
