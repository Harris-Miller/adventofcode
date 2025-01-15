const content = (await Bun.file('../inputs/2024/Day22/input.txt').text()).trim().split('\n').map(BigInt);

// console.log(content);

// eslint-disable-next-line no-bitwise
const mixNPrune = (secret: bigint, result: bigint): bigint => (secret ^ result) % BigInt(16777216);

const doTheThing = (secret: bigint): bigint => {
  let r = mixNPrune(secret, secret * BigInt(64));
  r = mixNPrune(r, r / BigInt(32));
  r = mixNPrune(r, r * BigInt(2048));
  return r;
};

const sumBigInt = (arr: bigint[]) => arr.reduce((a, b) => a + b, BigInt(0));

const part1 = sumBigInt(content.map(v => Array(2000).fill(undefined).reduce<bigint>(doTheThing, v)));

console.log(part1);
