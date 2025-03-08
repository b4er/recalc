# Backend (Haskell)

The main application for the backend follows the design of the
[Haskell Language Server][hls] (reactor pattern), it is built using a generic
way of handling [JSON-RPC][json-rpc] inspired by Servant's named routes.

At the core of the backend is the recalculation engine based on
[`build`][build][^1]. It is responsible for handling incremental recomputations
and dependency tracking for languages implementing the [`Recalc`][recalc-class]
interface. Its main function is [`recalc`][recalc-fun], used by:

1. The [`Recalc.Repl`][recalc-repl] driver (used for tests)
2. The [`Recalc.Univer`][recalc-univer] driver which provides a convenience
   function [`univerMain`][univer-main]

The latter will run a language server which implements the
[protocol][recalc-protocol] understood by the frontend.

## Packages

- **[`recalc-engine`](./haddock/recalc-engine/index.tml)**:
  The core recalculation engine, includes a (preliminary) implementation for a
  dependency map, the document store (keeping track of files and their sheets).
  It defines the `Recalc` interfaces and provides the [`Fetch`][recalc-fetch]
  monad.

- **[`recalc-server`](./haddock/recalc-server/index.html)**:
  A generic implementation for named handlers (from Servant-like protocol
  definition [^2]) of JSON-RPC for generic running language servers.

- **[`recalc-univer`](./haddock/recalc-server/index.html)**:
  A generic backend implementation for a language server talking to the Univer
  frontend. It deals with dispatching the right operations and implements the
  [`Univer.Protocol`][recalc-protocol].

<!-- Footnotes & References -->
  [^1]: Andrey Mokhov, Neil Mitchell, Simon Peyton Jones. [*Build Systems à la Carte*](https://dl.acm.org/doi/10.1145/3236774).

  [^2]: For more details refer to the blog post
        [*Named Routes in Servant*](https://www.tweag.io/blog/2022-02-24-named-routes)

  [hls]: https://github.com/haskell/haskell-language-server/blob/master/ghcide/src/Development/IDE/LSP/Server.hs
  [json-rpc]: https://www.jsonrpc.org/specification
  [build]: https://github.com/snowleopard/build
  [recalc-class]: ./haddock/recalc-engine/Recalc-Engine.html#t:Recalc
  [recalc-fun]: ./haddock/recalc-engine/Recalc-Engine.html#v:recalc
  [recalc-repl]: ./haddock/Recalc-Repl.html
  [recalc-fetch]: ./haddock/recalc-engine/Recalc-Engine.html#t:Fetch
  [recalc-protocol]: ./haddock/recalc-univer/Recalc-Univer-Protocol.html#v:SpreadsheetProtocol
  [recalc-univer]: ./haddock/recalc-univer/Recalc-Univer.html
  [univer-main]: ./haddock/recalc-univer/Recalc-Univer.html#v:univerMain
