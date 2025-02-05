# recalc: functional spreadsheet programming

**recalc** is a functional programming language embedded in a spreadsheet environment.

## Project Organization

- **[`recalc-engine/`](./lib/Recalc/Engine.hs)** (Haskell):
  The core recalculation engine based on [*Build Systems à la Carte*][build]. It handles
  incremental computations and dependency tracking for arbitrary (dependently typed)
  languages implementing the `Language` interface.

## Building the Project

This project uses Nix for a reproducible development environment and Cabal for building Haskell.

1. **Set up the development environment:**

   ```bash
   nix develop
   ```

   This will drop you into a shell with all necessary dependencies available where you can run the
   test suite:

   ```bash
   cabal test all
   ```

2. **Build the Haskell components:**

  Use the following within a dev shell:

   ```bash
   cabal build all
   ```

   Alternatively, you can use Nix directly:

   ```bash
   nix build
   ```

<!-- References -->

  [build]: https://dl.acm.org/doi/10.1145/3236774
