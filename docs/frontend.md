# Frontend (TypeScript)

The frontend is implemented as a standard [Web Extension][web-extension]:

The Extension automatically starts whenever a `*.rc` file is opened, it starts a backend
process (see `recalc-vscode.serverUri` configuration in configuration manifest) and
establishes a communication channel.

Each URI gets an associated Webview which is able to send URI-tagged messages to the
backend, the entry-point for the frontend starts a Spreadsheet UI using the
[Univer Sheet API][univer-sheet-api].

## Code Structure

The project is a standard NPM project with `esbuild.mjs` as the build script. The
frontend code is organized as a [standard Univer plugin][plugin-directory-structure].

<!--
- **`package.json`**: VSCode manifest file in which the extension
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
-->

<!-- References -->

  [web-extension]: https://code.visualstudio.com/api/extension-guides/webview
  [univer-sheet-api]: https://docs.univer.ai/en-US/guides/sheets/features/core/sheet-api
  [plugin-directory-structure]: https://docs.univer.ai/en-US/blog/anatomy#plugin-directory-structure
