# recalc: functional spreadsheet programming

![Lint Status](https://github.com/b4er/recalc/actions/workflows/nix-check.yaml/badge.svg?branch=main)
![Build Status](https://github.com/b4er/recalc/actions/workflows/nix-build.yaml/badge.svg?branch=main)

**recalc** is a functional programming language embedded in a spreadsheet environment.

## Project Organization

- **[`recalc-engine`](./lib/engine/Recalc/Engine.hs)** (Haskell):
  The core recalculation engine based on [*Build Systems à la Carte*][build]. It handles
  incremental computations and dependency tracking for arbitrary (dependently typed)
  languages implementing the `Language` interface.

- **[`recalc-lang`](./lib/lang/Recalc/Syntax)** (Haskell): The language implementation, a very basic
  dependently typed lambda calculus based on
  [*A tutorial implementation of a dependently typed lambda calculus*][lambdaPi].

- **[`recalc-server`](./lib/server/Recalc/Server.hs)** (Haskell): A generic implementation
  for named handlers (from Servant-like protocol definition) of json-rpc. And the concrete
  protocol definition.

- **[`recalc-server-exe`](./src/server/Main.hs)** (Haskell): The Spreadsheet backend
  json-rpc implementation.

- **[`recalc-spec`](./spec/README.md)** (Haskell): Full test suite for all Haskell tests.

- **[`recalc-ts-defs`](./src/ts/Main.hs)** (Haskell): TypeScript code generation for json-rpc
  and protocol type sharing with the Web Extension.

- **[`recalc-vscode`](./recalc-vscode)** (TypeScript): A Visual Studio Code web extension
  providing a frontend for editing and interacting with recalc sheets. The extension starts
  a Haskell process for backend computations.

  The frontend UI is built using the [Univer][univer-sheet-api] Sheet API.

## Building the Project

This project uses Nix for a reproducible development environment and Cabal for building Haskell.

If you only want to build the `.vsxi`, jump to (3):

1. **Set up the Development Environment:**

   Open a dev shell:

   ```bash
   nix develop
   ```

   This will drop you into a shell with all necessary dependencies available where you can run
   the test suite:

   ```bash
   cabal test
   npm --prefix recalc-vscode run tests
   ```

2. **Running the Web Extension in Development:**

   ```bash
   npm --prefix recalc-vscode install
   ```

   In a dev shell open the current directory (eg. `codium .`), launch the extension
   development host using (F5).

3. **Build the Web Extension:**

   ```bash
   nix build
   ```

   then to install:

   ```bash
   codium --install-extension "result/recalc-vscode-$(jq -r .version < recalc-vscode/package.json).vsix"
   ```

<!-- References -->

  [build]: https://dl.acm.org/doi/10.1145/3236774
  [lambdaPi]: https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf
  [univer-sheet-api]: https://docs.univer.ai/en-US/guides/sheets/features/core/sheet-api
