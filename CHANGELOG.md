# Revision history for recalc

## 1.2.0.0 -- 2025-03-28

* Minimal recalculation engine based on "Build systems à la carte", the engine
  supports
  * type inference,
  * type-directed elaboration,
  * evaluation,
  * handling of dependencies,
  * custom errors, values etc.
* `Recalc.Repl` for running single-sheet experiments in GHCi, and/or tests.
* Small functional language built on top of with
  * variables, lambda abstractions, applications
  * implicit arguments
  * cell references (ranges have tensor types)
  * hierarchy of types (sound)
  * annotations
  * dependent functions
  * dependent products
  * operators (unary `~`, `-`, and overloaded binary `*`, `+`, `-`)
  * minimal prelude: `not`, `and`, `or`, `mmult`
  * literals (Boolean values and numbers)

  The language is bi-directionally type checked and does term elaboration
  (inserting witnesses for implicit arguments).

  It also provides a simple, HOAS interpreter.
* Type-safe routing for TypeScript from Servant-like protocols; implemented
  generic server using the reactor pattern.
* Server-side implementation used by the TypeScript frontend that handles rpc:
  value ranges, sheet insertion, sheet removal, worksheet order, worksheet name,
  and sheet-defined functions (dummy handler).

  The frontend provides (in addition to Univer functionality):
  * cell-diagnostics for errors (parse, typing..) and inferred types, and
  * sheet-defined function editor.
* Full test suite (cabal and npm, including end-to-end tests).
* Static builds for `.vsix` packaging and project structure adjustments.
* CI/CD, GitHub Pages documentation ..
