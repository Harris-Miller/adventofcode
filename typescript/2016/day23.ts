import { AssemBunny } from './assembunny';

const content = (await Bun.file('../inputs/2016/Day23/input.txt').text()).trim().split('\n');

const asm = new AssemBunny(content, { a: 7 });

asm.run();

console.log(asm.regA);
