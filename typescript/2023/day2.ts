import { drop, isNil, max, product, sum } from 'ramda';

type Cube = { color: string; num: number };
type Group = Cube[];
type Game = { gameNum: number; groups: Group[] };

const content = (await Bun.file('2023/inputs/Day2/input.txt').text()).split('\n').filter(Boolean);

// console.log(content);

const parseGame = (str: string): Game => {
  const [gameName, groupsStr] = str.split(': ');

  const gameNum = Number.parseInt(drop(5, gameName), 10);

  const groups = groupsStr.split('; ').map(group =>
    group.split(', ').map(s => {
      const [num, color] = s.split(' ');
      return { color, num: Number.parseInt(num, 10) };
    }),
  );

  return { gameNum, groups };
};

const games = content.map(parseGame);

// part 1
const isGroupPossible = (toCheckAgainst: Group) => (group: Group) =>
  group.every(cube => {
    const found = toCheckAgainst.find(({ color }) => color === cube.color);
    if (isNil(found)) return false;
    return cube.num <= found.num;
  });

const toCheckAgainst: Group = [
  { color: 'red', num: 12 },
  { color: 'green', num: 13 },
  { color: 'blue', num: 14 },
];

const possibleGameNums = games
  .map(({ gameNum, groups }: Game) => ({
    gameNum,
    isPossible: groups.every(isGroupPossible(toCheckAgainst)),
  }))
  .filter(x => x.isPossible)
  .map(({ gameNum }) => gameNum);

const r1 = sum(possibleGameNums);

console.log(r1);

// part 2
const calcGamePower = (game: Game) => {
  const mins = game.groups.flat().reduce<Record<string, number>>((acc, cube) => {
    // eslint-disable-next-line no-param-reassign
    acc[cube.color] = max(acc[cube.color] ?? 0, cube.num);
    return acc;
  }, {});

  return product(Object.values(mins));
};

const r2 = sum(games.map(calcGamePower));

console.log(r2);
