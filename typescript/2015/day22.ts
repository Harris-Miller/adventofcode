import { dijkstra } from 'fp-search-algorithms';
import { add, assoc, assocPath, fromPairs, identity, modify, pipe, toPairs } from 'ramda';
import { match } from 'ts-pattern';

import { unzip } from '../lib/fp';

type InstantSpell = 'Drain' | 'MagicMissile';
type EffectSpell = 'Poison' | 'Recharge' | 'Shield';
type SpellName = EffectSpell | InstantSpell;

const allSpells: SpellName[] = ['MagicMissile', 'Drain', 'Shield', 'Poison', 'Recharge'];

const spellCosts: Record<SpellName | '', number> = {
  '': 0,
  Drain: 73,
  MagicMissile: 53,
  Poison: 173,
  Recharge: 229,
  Shield: 113,
};

type GameState = {
  bossHit: number;
  bossHp: number;
  // emptyString just for initial state
  effects: Record<EffectSpell, number>;
  playerArmour: number;
  playerHp: number;
  playerMp: number;
  spell: SpellName | '';
  totalMana: number;
};

const sample: GameState = {
  bossHit: 8,
  bossHp: 14,
  effects: { Poison: 0, Recharge: 0, Shield: 0 },
  playerArmour: 0,
  playerHp: 10,
  playerMp: 250,
  spell: '',
  totalMana: 0, // TODO: fix
};

// TODO: make not the same as sample
const input: GameState = {
  bossHit: 8,
  bossHp: 14,
  effects: { Poison: 0, Recharge: 0, Shield: 0 },
  playerArmour: 0,
  playerHp: 10,
  playerMp: 250,
  spell: '',
  totalMana: 0, // TODO: fix
};

const setEffect = (spellName: EffectSpell) => (gameState: GameState) => {
  const timer = match(spellName)
    .with('Shield', 'Poison', () => 6)
    .with('Recharge', () => 5)
    .exhaustive();

  return assocPath(['effects', spellName], timer, gameState);
};

const processEffect = ([spellName, timer]: [EffectSpell, number]): [
  [EffectSpell, number],
  (gameState: GameState) => GameState,
] => {
  if (timer === 0) return [[spellName, 0], identity];

  const fn = match(spellName)
    .with('Shield', () => {
      if (timer === 6) return assoc('playerArmour', 7);
      if (timer === 1) return assoc('playerArmour', 0);
      return identity;
    })
    .with('Poison', () => modify('bossHp', add(-3)))
    .with('Recharge', () => modify('playerMp', add(101)))
    .exhaustive();

  return [[spellName, timer - 1], fn];
};

const processEffects = (gameState: GameState): GameState => {
  const [nextEffects, fns] = unzip(toPairs(gameState.effects).map(processEffect));
  const nextGameState = fns.reduce((acc, fn) => fn(acc), gameState);
  return assoc('effects', fromPairs(nextEffects), nextGameState);
};

const useSpell = (name: SpellName) =>
  pipe(
    assoc('spell', name)<GameState>,
    modify('totalMana', add(spellCosts[name])),
    modify('playerMp', (x: number) => x - spellCosts[name]),
    (gameState: GameState) => {
      switch (name) {
        case 'MagicMissile':
          return modify('bossHp', add(-4), gameState);
        case 'Drain':
          return pipe(modify('bossHp', add(-2))<GameState>, modify('playerHp', add(2)))(gameState);
        default:
          return setEffect(name)(gameState);
      }
    },
  );

const playerTurn = (isHard: boolean, gameState: GameState) => (spell: SpellName) =>
  pipe((gs: GameState) => (isHard ? modify('playerHp', add(-1), gs) : gs), useSpell(spell))(gameState);

const bossTurn = (gameState: GameState) =>
  modify('playerHp', add(gameState.playerArmour - gameState.bossHit), gameState);

const getNextStates =
  (isHard: boolean) =>
  (gameState: GameState): GameState[] => {
    if (gameState.playerHp <= 0) return [];

    const intermediateState = processEffects(gameState);
    const { playerMp, effects } = intermediateState;
    const nextSpells = allSpells.filter(spell => effects[spell as EffectSpell] === 0 && spellCosts[spell] <= playerMp);

    return nextSpells.map(pipe(playerTurn(isHard, intermediateState), processEffects, bossTurn));
  };

const getCost = (_: GameState, nextGameState: GameState) => spellCosts[nextGameState.spell];

const found = (gameState: GameState) => gameState.bossHp <= 0;

// TODO: this stopped working, why?

const part1 = dijkstra(getNextStates(false), getCost, found, sample);
console.log(part1?.[0]);

const part2 = dijkstra(getNextStates(true), getCost, found, input);
console.log(part2?.[0]);
