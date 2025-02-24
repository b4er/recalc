# Backend (Haskell)

The main application for the backend follows the design of the
[Haskell Language Server][hls] (reactor pattern), it is built using a generic
way of handling [JSON-RPC][json-rpc] inspired by Servant's
[named routes][named-routes].

At the core of the backend is the recalculation engine based on
[`build`][build][^1]. It is responsible for handling incremental recomputations
and dependency tracking for languages implementing the `Language` interface.

To deal with multiple frontends (see for example [`Recalc.Repl`][recalc-repl])
the backend uses the typeclass `class Input dat` which specifies how to extract
a term (or value) from an input.

The Univer frontend uses [`CellData`][recalc-celldata] in the
[protocol][recalc-protocol].

## Code Structure

- **[`Recalc.Engine`](./haddock/Recalc-Engine.html)**:
  The core recalculation engine, includes a (preliminary) implementation for a
  dependency map, the document store (keeping track of files and their sheets),
  the `Language` interface and `Fetch` monad.

- **[`Recalc.Repl`](./haddock/Recalc-Repl.html)**:
  A very simple frontend/collection of utility functions to interact with a
  spreadsheet using GHCi.

- **[`Recalc.Semantics`](./haddock/Recalc-Semantics.html)**:
  The core language implementation (evaluation and bi-directional typechecking).

- **[`Recalc.Server`](./haddock/Recalc-Server.html)**:
  A generic implementation for named handlers (from Servant-like protocol
  definition) of JSON-RPC.
  And the concrete protocol definition [`Server.Protocol`][recalc-protocol] for
  the Univer frontend.

- **[`Recalc.Syntax`](./haddock/Recalc-Syntax-Term.html)**:
  The core language syntax: the surface AST and parser

- **`spec`**: Full test suite for all Haskell tests (see [Development Guide][development]).

<!-- Footnotes & References -->
  [^1]: Andrey Mokhov, Neil Mitchell, Simon Peyton Jones. [*Build Systems à la Carte*](https://dl.acm.org/doi/10.1145/3236774).

  [hls]: https://github.com/haskell/haskell-language-server
  [json-rpc]: https://www.jsonrpc.org/specification
  [named-routes]: https://www.tweag.io/blog/2022-02-24-named-routes
  [build]: https://github.com/snowleopard/build
  [recalc-repl]: ./haddock/Recalc-Repl.html
  [recalc-celldata]: ./haddock/Recalc-Server-Protocol.html#v:CellData
  [recalc-protocol]: ./haddock/Recalc-Server-Protocol.html#v:SpreadsheetProtocol
  [development]: ./development.md
