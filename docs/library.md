# Library

The backend components (recalculation engine, server routing and language
implementation) are decoupled and can easily be re-used.

The Haskell code could be split into different libraries:

- `recalc-engine`: generic engine
- `recalc-server`: generic server
- `recalc-univer`: types and protocol + generic implementation
- `recalc-exe`: defines language implementation and uses higher-order `main`
  from `recalc-univer` (or different frontend)

However, while getting static builds to work I refactored all code into a
single monolithic app.

In the meantime, the easiest is to just modify/replace the modules
[`Recalc.Syntax.Term`](./haddock/Recalc-Syntax-Term.html),
[`Recalc.Syntax.Parser`](./haddock/Recalc-Syntax-Parser.html), and
[`Recalc.Semantics`](./haddock/Recalc-Semantics.html).
