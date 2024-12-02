const content = await Bun.file('../inputs/2020/Day6/sample.txt').text();

// console.log(content);

const processFile = () => {
  const byLine = content.split('\n');
  let groupTotals = 0;
  let acc = new Set();

  for (const line of byLine) {
    if (line === '') {
      groupTotals += acc.size;
      acc = new Set();
      continue;
    }

    const byChar = line.split('');
    byChar.forEach(c => acc.add(c));
  }

  return groupTotals;
};

const results = processFile();

// console.log(results);

const parseToGroups = () => {
  const groups = content
    .trim()
    .split('\n\n')
    .map(group => group.split('\n'));

  return groups;
};

const processGroup = (group: string[]) => {
  const combined = group.reduce((acc, x) => acc + x, ''); // concat
  const sepByChar = combined.split('');
  const unique = new Set(sepByChar);
  return unique.size;
};

const processGroup2 = (group: string[]) => {
  const uniqByRow = group.map(x => new Set(x.split('')));
  const [first, ...rest] = uniqByRow;
  const allInCommon = rest.reduce((acc, x) => acc.intersection(x), first);
  return allInCommon.size;
};

const process2 = () => {
  const groups = parseToGroups();
  const countByGroup = groups.map(processGroup2);
  const r = countByGroup.reduce((acc, x) => acc + x, 0); // sum
  return r;
};

const results2 = process2();

console.log(results2);
