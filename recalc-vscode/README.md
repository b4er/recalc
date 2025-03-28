# TypeScript Frontend (Web Extension for VSCode)

This is a frontend for [`recalc`](../README.md) implemented as a
[Web Extension](https://code.visualstudio.com/api/extension-guides/webview):

The Extension automatically starts whenever a `*.rc` file is opened, it starts a backend
process (see `recalc-vscode.serverUri` configuration in configuration manifest) and
establishes a communication channel.

Each URI gets an associated Webview which is able to send URI-tagged messages to the
backend, the entry-point for the frontend starts a Spreadsheet UI using the
[Univer Sheet API][univer-sheet-api].

## Project Organization

The project is a standard NPM project with [`esbuild.mjs`](./esbuild.mjs) as the build
script. The linter config [`eslint.config.js`](./eslint.config.js) is not available
to the `eslint` Git-Hooks, so it needs to be manually run (`npm run lint` in a devShell).

- **[`package.json`](./package.json)**: VSCode manifest file in which the extension
  (custom editor, commands etc.) are declared.
- **[`hs`](./hs/Main.hs)**: Source code for TypeScript generation of protocols.
- **[`recalc-ts-defs.cabal`](./recalc-ts-defs.cabal)**: This is a separate .cabal,
  from [`recalc.cabal`](../recalc.cabal) allowing (static) build that don't include
  the template-haskell.
- **[`src/extension/extension.ts`](./src/extension/extension.ts)**: The main file for the
  custom editor.
- **[`src/frontend`](./src/frontend)**: Sub-divided like a
  [standard Univer plugin][plugin-directory-structure], consists of
  [`controllers`](./src/frontend/controllers), [`services`](./src/frontend/services),
  and [`views`](./src/frontend/views).
- **[`src/frontend/index.ts`](./src/frontend/index.ts)**: The main file for the frontend.
- **[`src/rpc`](./src/frontend/rpc)**: Json-rpc client implementation.
- **[`src/test`](./src/test)**: The TypeScript test suite for the, automatically
  collects `*.test.ts`.
- **[`tsconfig.json`](./tsconfig.json)**: TypeScript transpiler configuration.

## Building the Project

Refer to [README.md](../README.md#building-the-project).

<!-- References -->

  [univer-sheet-api]: https://docs.univer.ai/en-US/guides/sheets/features/core/sheet-api
  [plugin-directory-structure]: https://docs.univer.ai/en-US/blog/anatomy#plugin-directory-structure
