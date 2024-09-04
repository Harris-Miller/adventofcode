import { AssemBunny } from './assembunny';

const content = (await Bun.file('../inputs/2016/Day12/input.txt').text()).trim().split('\n');

const asm = new AssemBunny(content);

asm.run();

console.log(asm.regA);
