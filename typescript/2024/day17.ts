/* eslint-disable no-bitwise */
/* eslint-disable operator-assignment */
/* eslint-disable no-param-reassign */
import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';

const content = (await Bun.file('../inputs/2024/Day17/input.txt').text()).trim();

// console.log(content);

type Registers = { a: number; b: number; c: number };

const parse = (str: string): { program: number[]; registers: Registers } => {
  const [registers, program] = str.split('\n\n');

  const rr = registers.split('\n').map(line => parseInt10(R.drop(12, line)));

  return {
    program: R.drop(9, program).split(',').map(parseInt10),
    registers: {
      a: rr[0],
      b: rr[1],
      c: rr[2],
    },
  };
};

const parsed = parse(content);

// console.log(parsed);

const getComboOperand = (registers: Registers, index: number) => {
  switch (index) {
    case 0:
    case 1:
    case 2:
    case 3:
      return index;
    case 4:
      return registers.a;
    case 5:
      return registers.b;
    case 6:
      return registers.c;
    default:
      throw new Error(`getOperand non-exhaustive :: ${index}}`);
  }
};

// eslint-disable-next-line complexity
const doOp = (registers: Registers, program: number[], pointer: number, outputs: number[]): number[] => {
  // console.log(pointer);
  if (pointer >= program.length) return outputs;

  const opcode = program[pointer];
  const operand = program[pointer + 1];

  switch (opcode) {
    case 0: {
      registers.a = Math.trunc(registers.a / 2 ** getComboOperand(registers, operand));
      break;
    }
    case 1: {
      registers.b = registers.b ^ operand;
      break;
    }
    case 2: {
      registers.b = getComboOperand(registers, operand) % 8;
      break;
    }
    case 3: {
      if (registers.a !== 0) {
        return doOp(registers, program, operand, outputs);
      }
      break;
    }
    case 4: {
      registers.b = registers.b ^ registers.c;
      break;
    }
    case 5: {
      outputs.push(getComboOperand(registers, operand) % 8);
      break;
    }
    case 6: {
      registers.b = Math.trunc(registers.a / 2 ** getComboOperand(registers, operand));
      break;
    }
    case 7: {
      registers.c = Math.trunc(registers.a / 2 ** getComboOperand(registers, operand));
      break;
    }
    default:
      throw new Error(`doOp non-exhaustive :: ${opcode}}`);
  }
  return doOp(registers, program, pointer + 2, outputs);
};

const input1 = R.clone(parsed);

const r1 = doOp(input1.registers, input1.program, 0, []);

console.log(r1.join(','));

let a = 2025;
// eslint-disable-next-line no-constant-condition
while (true) {
  const input = R.clone(parsed);
  input.registers.a = a;
  const r2 = doOp(input.registers, input.program, 0, []);
  if (R.equals(input.program, r2)) {
    console.log(a);
    break;
  }
  a += 1;
}
