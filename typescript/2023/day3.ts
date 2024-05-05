import { head, last, range, sum } from 'ramda';

const content = await Bun.file('../../inputs/2023/Day3/input.txt').text();

type Pos = { col: number; line: number };

type Sym = Pos & {
  char: string;
};

type PartNum = {
  colRange: number[];
  eCol: number;
  line: number;
  num: number;
  posSurrounding: Pos[];
  sCol: number;
};

const rNum = /^(\d)+/;

const serializePos = (pos: Pos) => `${pos.line},${pos.col}`;

const calcNeighborsAroundPartNum = (line: number, sCol: number, eCol: number): Pos[] => {
  // eCol + 2 because `end` in `range(start, end)` is exclusive
  const colRange = range(sCol - 1, eCol + 2);
  const top = colRange.map(i => ({ col: i, line: line - 1 }));
  const bottom = colRange.map(i => ({ col: i, line: line + 1 }));
  return [{ col: sCol - 1, line }, { col: eCol + 1, line }, ...top, ...bottom];
};

class CustomParser {
  private line = 1;
  private col = 1;
  private partNums: PartNum[] = [];
  private symbols: Sym[] = [];

  constructor(private input: string) {}

  handlePeriod() {
    this.input = this.input.substring(1);
    this.col += 1;
  }

  handleNewline() {
    this.input = this.input.substring(1);
    this.col = 1;
    this.line += 1;
  }

  parseSymOrPartNum() {
    const maybeNumber = this.input.match(rNum);

    if (maybeNumber) {
      const [num] = maybeNumber;
      const colRange = range(this.col, this.col + num.length);
      const { line } = this;
      const sCol = head(colRange)!;
      const eCol = last(colRange)!;
      this.partNums.push({
        colRange,
        eCol,
        line,
        num: Number.parseInt(num, 10),
        posSurrounding: calcNeighborsAroundPartNum(line, sCol, eCol),
        sCol,
      });
      this.input = this.input.substring(num.length);
      this.col += num.length;
    } else {
      const char = this.input.charAt(0);
      this.symbols.push({
        char,
        col: this.col,
        line: this.line,
      });
      this.input = this.input.substring(1);
      this.col += 1;
    }
  }

  parse() {
    while (this.input.length) {
      const headChar = this.input.charAt(0);
      switch (headChar) {
        case '.':
          this.handlePeriod();
          break;
        case '\n':
          this.handleNewline();
          break;
        default:
          this.parseSymOrPartNum();
      }
    }

    return { partNums: this.partNums, symbols: this.symbols };
  }
}

const filterPartNums =
  (symbolPosSet: Set<string>) =>
  (partNum: PartNum): boolean => {
    const partNumPoses = partNum.posSurrounding.map(serializePos);
    for (const pos of partNumPoses) {
      if (symbolPosSet.has(pos)) return true;
    }
    return false;
  };

// console.log(content);

const { partNums, symbols } = new CustomParser(content).parse();

console.log(partNums.length);
console.log(symbols.length);

const symbolPosSet = new Set<string>(symbols.map(serializePos));

const filteredPartNums = partNums.filter(filterPartNums(symbolPosSet));

console.log(sum(filteredPartNums.map(x => x.num)));
