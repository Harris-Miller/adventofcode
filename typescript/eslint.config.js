/* eslint-disable sort-keys-fix/sort-keys-fix */
import harrisConfigBase from 'eslint-config-harris/base';
import globals from 'globals';

/** @type {import('eslint').Linter.FlatConfig[]} */
const eslintConfig = [
  ...harrisConfigBase,
  {
    languageOptions: {
      globals: {
        ...globals.nodeBuiltin,
        Bun: true,
      },
    },
  },
  {
    rules: {
      'no-console': 'off',
      'no-continue': 'off',
      'line-comment-position': 'off',
      'no-constant-condition': 'error',
      'no-plusplus': 'off',
    },
  },
  {
    files: ['**/*.ts', '**/*.mts', '**/*.tsx'],
    rules: {
      '@typescript-eslint/explicit-module-boundary-types': 'off',
      '@typescript-eslint/no-non-null-assertion': 'off',
      '@typescript-eslint/no-unnecessary-condition': ['error', { allowConstantLoopConditions: true }],
    },
  },
  {
    ignores: ['node_modules/'],
  },
];

export default eslintConfig;
