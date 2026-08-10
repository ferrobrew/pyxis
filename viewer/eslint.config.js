import js from '@eslint/js';
import globals from 'globals';
import reactHooks from 'eslint-plugin-react-hooks';
import reactRefresh from 'eslint-plugin-react-refresh';
import betterTailwind from 'eslint-plugin-better-tailwindcss';
import tseslint from 'typescript-eslint';
import { defineConfig, globalIgnores } from 'eslint/config';

export default defineConfig([
  globalIgnores(['dist', '.vite']),
  {
    files: ['**/*.{ts,tsx}'],
    extends: [
      js.configs.recommended,
      tseslint.configs.recommended,
      reactHooks.configs.flat.recommended,
      reactRefresh.configs.vite,
      // `recommended-error` so the stylistic warnings are treated as errors
      // too: lint stays a binary signal.
      betterTailwind.configs['recommended-error'],
    ],
    languageOptions: {
      ecmaVersion: 2020,
      globals: globals.browser,
    },
    settings: {
      // Point the Tailwind rule at the v4 CSS entry point so custom tokens
      // (e.g. `text-fg-subtle`) resolve against the theme.
      'better-tailwindcss': {
        entryPoint: 'src/index.css',
      },
    },
    rules: {
      'react-refresh/only-export-components': ['warn', { allowConstantExport: true }],
      'react-hooks/exhaustive-deps': 'error',
      'react-hooks/set-state-in-effect': 'error',
      // Prettier owns line wrapping; better-tailwindcss's line-wrapping rule
      // fights it (prettier collapses long class strings, the plugin wants
      // them multi-line). Keep the correctness rules (unknown/concatenated/
      // deprecated/duplicate/conflicting classes, canonical forms, class
      // order) as errors; line wrapping stays prettier's job.
      'better-tailwindcss/enforce-consistent-line-wrapping': 'off',
    },
  },
]);