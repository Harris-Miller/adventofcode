import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';

const regs = ['a', 'b', 'c', 'd'];

type RunOpts = {
  noOptimization?: boolean;
  steps?: number;
};

export class AssemBunny {
  regA: number;
  regB: number;
  regC: number;
  regD: number;

  instructions: string[];
  len: number;
  pointer = 0;
  indexToBeginOptimization: number | undefined;

  constructor(instructions: string[], init: Partial<Record<'a' | 'b' | 'c' | 'd', number>> = {}) {
    this.instructions = instructions;
    this.len = instructions.length;
    this.regA = init.a ?? 0;
    this.regB = init.b ?? 0;
    this.regC = init.c ?? 0;
    this.regD = init.d ?? 0;
  }

  run(opts: RunOpts = {}) {
    const { noOptimization = false, steps = Infinity } = opts;

    let counter = steps;
    while (this.pointer < this.len && counter > 0) {
      if (noOptimization) {
        this.processInst();
      } else if (this.pointer === this.indexToBeginOptimization) {
        if (this.optimize()) {
          this.processInst();
        }
      } else {
        this.processInst();
      }

      counter -= 1;
    }
  }

  optimize() {
    // collect instructs between pointer and indexToBeginOptimization
    const sub = this.instructions.slice(this.pointer, this.indexToBeginOptimization);
    // the final instruction will always be a jnz
    const jnz = R.last(sub)!;

    const [, value] = this.parseInst(jnz);
    const [, offset] = this.parseJnz(value);

    if (offset === -2) {
      // assume within sub is a `dec` is the same as jnz
      // to optimize, get value at reg, then set to 0
      // add to current reg value for the `inc` op
      const regWithAddVal = R.drop(4, sub.find(s => s.startsWith('dec'))!);
      const addVal = this.getReg(regWithAddVal);
      const regToAddTo = R.drop(4, sub.find(s => s.startsWith('inc'))!);
      const val = this.getReg(regToAddTo);
      this.setReg(regToAddTo, val + addVal);
      this.pointer = this.indexToBeginOptimization! + 1;
      this.indexToBeginOptimization = undefined;
      return true;
    }

    // return false to say no optimization happened
    // this will trigger processInst()
    return false;
  }

  lookupHead() {
    const sub = this.instructions.slice(this.pointer);
    const jnzIndex = sub.findIndex(inst => inst.startsWith('jnz'));
    const parsed = R.drop(4, sub[jnzIndex]).split(' ');

    const offset = (() => {
      switch (parsed[1]) {
        case 'a':
        case 'b':
        case 'c':
        case 'd':
          return this.getReg(parsed[1]);
        default:
          return parseInt10(parsed[1]);
      }
    })();

    this.indexToBeginOptimization = jnzIndex - offset;
  }

  processInst() {
    const str = this.instructions[this.pointer];
    const [inst, value] = this.parseInst(str);

    // since jnz moves the pointer, do job and return
    if (inst === 'jnz') {
      this.jnz(value);
      return;
    }

    // else, process and increment pointer
    switch (inst) {
      case 'tgl':
        this.tgl(value);
        break;
      case 'cpy':
        this.cpy(value);
        break;
      case 'inc':
        this.inc(value);
        break;
      case 'dec':
        this.dec(value);
        break;
      default:
        throw new Error('Exhaustive inst invariant');
    }
    this.pointer += 1;
  }

  tgl(value: string) {
    const val = (() => {
      switch (value) {
        case 'a':
        case 'b':
        case 'c':
        case 'd':
          return this.getReg(value);
        default:
          return parseInt10(value);
      }
    })();

    const positionToUpdate = this.pointer + val;

    // bail is position is out-of-range
    if (positionToUpdate < 0 || positionToUpdate >= this.len) return;

    const inst = this.instructions[positionToUpdate];

    switch (R.take(3, inst)) {
      case 'inc':
        this.instructions[positionToUpdate] = `dec${R.drop(3, inst)}`;
        break;
      case 'dec':
      case 'tgl':
        this.instructions[positionToUpdate] = `inc${R.drop(3, inst)}`;
        break;
      case 'jnz':
        this.instructions[positionToUpdate] = `cpy${R.drop(3, inst)}`;
        break;
      case 'cpy':
        this.instructions[positionToUpdate] = `jnz${R.drop(3, inst)}`;
        break;
      default:
        throw new Error('Exhaustive tgl invariant');
    }
  }

  cpy(value: string) {
    const [val, reg] = value.split(' ') as [string, string];

    // cpy may be invalid due to tgl, eg `cpy 1 2`
    // in this case, just bail
    if (!regs.includes(reg)) return;

    switch (val) {
      case 'a':
      case 'b':
      case 'c':
      case 'd':
        this.setReg(reg, this.getReg(val));
        break;
      default:
        this.setReg(reg, parseInt10(value));
    }
  }

  inc(value: string) {
    const reg = value;

    // cpy may be invalid due to tgl, eg `cpy 1 2`
    // in this case, just bail
    if (!regs.includes(reg)) return;

    this.setReg(reg, this.getReg(reg) + 1);
  }

  dec(value: string) {
    const reg = value;
    this.setReg(reg, this.getReg(reg) - 1);
  }

  jnz(value: string) {
    const [val, offset] = this.parseJnz(value);

    if (val === 0) {
      this.pointer += 1;
    } else {
      this.pointer += offset;
    }
  }

  getReg(reg: string): number {
    switch (reg) {
      case 'a':
        return this.regA;
      case 'b':
        return this.regB;
      case 'c':
        return this.regC;
      case 'd':
        return this.regD;
      default:
        throw new Error('Exhaustive getReg invariant');
    }
  }

  setReg(reg: string, value: number) {
    switch (reg) {
      case 'a':
        this.regA = value;
        break;
      case 'b':
        this.regB = value;
        break;
      case 'c':
        this.regC = value;
        break;
      case 'd':
        this.regD = value;
        break;
      default:
        throw new Error('Exhaustive setReg invariant');
    }
  }

  parseInst(str: string): [string, string] {
    const parsed = R.splitAt(4, str);
    const inst = parsed[0].trim();
    // eslint-disable-next-line @typescript-eslint/prefer-destructuring
    const value = parsed[1];
    return [inst, value];
  }

  parseJnz = (value: string): [number, number] => {
    const parsed = value.split(' ') as [string, string];
    const val = (() => {
      switch (parsed[0]) {
        case 'a':
        case 'b':
        case 'c':
        case 'd':
          return this.getReg(parsed[0]);
        default:
          return parseInt10(parsed[0]);
      }
    })();
    const offset = (() => {
      switch (parsed[1]) {
        case 'a':
        case 'b':
        case 'c':
        case 'd':
          return this.getReg(parsed[1]);
        default:
          return parseInt10(parsed[1]);
      }
    })();

    return [val, offset];
  };
}
