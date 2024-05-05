import { drop, intersection, isEmpty, isNotNil, range, sum } from 'ramda';

const content = (await Bun.file('../../inputs/2023/Day4/input.txt').text()).split('\n').filter(s => !isEmpty(s));

const parseNumbers = (str: string) =>
  str
    .split(' ')
    .filter(x => !isEmpty(x))
    .map(x => Number.parseInt(x, 10));

const parseCard = (cardStr: string) => {
  const [cardName, rest] = cardStr.split(': ');
  const cardNum = Number.parseInt(drop(5, cardName), 10);
  const [winners, holding] = rest.split(' | ').map(parseNumbers);

  return { cardNum, holding, winners };
};

type Card = ReturnType<typeof parseCard>;

const parsed: Card[] = content.map(parseCard);

const determineWinners = (cards: Card[]) =>
  cards
    .map(card => intersection(card.winners, card.holding).length)
    .filter(x => x !== 0)
    .map(x => 2 ** (x - 1));

// part 1
console.log(sum(determineWinners(parsed)));

const processCards = (cards: Card[]): number[] => {
  const cardsWon = Object.fromEntries(
    cards
      .map(card => [card.cardNum, intersection(card.winners, card.holding).length] as [number, number])
      .filter(([, l]) => l !== 0)
      .map(([k, v]) => [k, range(k + 1, k + v + 1)] as [number, number[]]),
  );

  let acc: number[] = cards.map(x => x.cardNum);
  let xs: number[] = cards.map(x => x.cardNum);

  while (xs.length) {
    xs = xs
      .map(x => cardsWon[x])
      .filter(isNotNil)
      .flat();

    acc = acc.concat(xs);
  }

  return acc;
};

// part 2
console.log(processCards(parsed).length);
