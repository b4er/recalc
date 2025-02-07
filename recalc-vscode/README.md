# TypeScript Frontend (Web Extension for VSCode)

This is a frontend for [`recalc`](../README.md) implemented as a
[Web Extension](https://code.visualstudio.com/api/extension-guides/webview).

## Project Organization

- **[`esbuild.mjs`](./esbuild.mjs)**: esbuild script to build the custom editor.
- **[`eslint.config.js`](./eslint.config.js)**: linter config.
- **[`package.json`](./package.json)**: VSCode manifest file in which the extension
  (custom editor, commands etc.) are declared.
- **[`src/extension/extension.ts`](./src/extension/extension.ts)**: The main file for the
  custom editor.
- **[`src/frontend/index.ts`](./src/frontend/index.ts)**: The main file for the frontend.
- **[`tsconfig.json`](./tsconfig.json)**: TypeScript transpiler configuration.
