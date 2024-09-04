import * as R from 'ramda';

import { parseInt10 } from '../lib/fp';

type Register = 'a' | 'b' | 'c' | 'd';

const regs = ['a', 'b', 'c', 'd'];

export class AssemBunny {
  public regA: number;
  public regB: number;
  public regC: number;
  public regD: number;

  private instructions: string[];
  private len: number;
  private pointer = 0;

  constructor(instructions: string[], init: Partial<Record<Register, number>> = {}) {
    this.instructions = instructions;
    this.len = instructions.length;
    this.regA = init.a ?? 0;
    this.regB = init.b ?? 0;
    this.regC = init.c ?? 0;
    this.regD = init.d ?? 0;
  }

  public run() {
    while (this.pointer < this.len) {
      const { pointer } = this;
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const inst = this.instructions[this.pointer];
      this.processInst(this.instructions[pointer]);
      // console.log(pointer, [this.regA, this.regB, this.regC, this.regD], inst);
    }
  }

  private processInst(inst: string) {
    const parsed = R.splitAt(4, inst);
    const key = parsed[0].trim();
    // eslint-disable-next-line @typescript-eslint/prefer-destructuring
    const value = parsed[1];

    // since jnz moves the pointer, do job and return
    if (key === 'jnz') {
      this.jnz(value);
      return;
    }

    // else, process and increment pointer
    switch (key.trim()) {
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

  private tgl(value: string) {
    const val = (() => {
      switch (value) {
        case 'a':
        case 'b':
        case 'c':
        case 'd':
          return this.getReg(value as Register);
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

  private cpy(value: string) {
    const [val, reg] = value.split(' ') as [string, Register];

    // cpy may be invalid due to tgl, eg `cpy 1 2`
    // in this case, just bail
    if (!regs.includes(reg)) return;

    switch (val) {
      case 'a':
      case 'b':
      case 'c':
      case 'd':
        this.setReg(reg, this.getReg(val as Register));
        break;
      default:
        this.setReg(reg, parseInt10(value));
    }
  }

  private inc(value: string) {
    const reg = value as Register;

    // cpy may be invalid due to tgl, eg `cpy 1 2`
    // in this case, just bail
    if (!regs.includes(reg)) return;

    this.setReg(reg, this.getReg(reg) + 1);
  }

  private dec(value: string) {
    const reg = value as Register;
    this.setReg(reg, this.getReg(reg) - 1);
  }

  private jnz(value: string) {
    const parsed = value.split(' ') as [string, string];
    const val = (() => {
      switch (parsed[0]) {
        case 'a':
        case 'b':
        case 'c':
        case 'd':
          return this.getReg(parsed[0] as Register);
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
          return this.getReg(parsed[1] as Register);
        default:
          return parseInt10(parsed[1]);
      }
    })();

    if (val === 0) {
      this.pointer += 1;
    } else {
      this.pointer += offset;
    }
  }

  private getReg(reg: Register): number {
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

  private setReg(reg: Register, value: number) {
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
}
