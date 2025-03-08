# Library

The backend components (recalculation engine, server routing and language
implementation) are decoupled and can readily be re-used.

Since there is only one actual frontend, the most common usage will be:

```haskell
import Recalc.Univer

data Term = ...

instance Recalc Term where
  -- language semantics

instance UniverRecalc Term where
  -- error pretty-printing
  -- sheet-defined function handling (optional)

main :: IO ()
main = univerMain @Term env0
```

The [Recalc][recalc] instance specifies how to

- parse formulas and values (simple terms),
- compute the dependencies of a term,
- infer types given the type(s) of other cells (using
  [`fetchType`][fetch-type]),
- and evaluate a term (again given the values of other cells, this time using
  [`fetchValue`][fetch-value]).

The [`UniverRecalc`][univer-recalc] instance specifies how to

- display errors (by defining `errorAnnotation`),
- and (optionally) how a sheet-defined function extends the environment.

For concrete implementations refer to [engine tests][engine-spec],
or the [core language][recalc-lang] implementation (described [here][core]).

---

The TypeScript frontend currently is not in a library form, the easiest
will be to run the original extension and configure the language server
(by setting `recalc-vscode.serverUri` in your [`settings.json`][settings]).

<!-- Footnotes & References -->

  [recalc]: ./haddock/recalc-engine/Recalc-Engine.html#t:Recalc
  [fetch-type]: ./haddock/recalc-engine/Recalc-Engine.html#v:fetchType
  [fetch-value]: ./haddock/recalc-engine/Recalc-Engine.html#v:fetchValue
  [univer-recalc]: ./haddock/recalc-engine/Recalc-Engine.html#t:Recalc
  [engine-spec]: /haddock/recalc-engine/recalc-engine-spec/src/Recalc.EngineSpec.html#line-206
  [recalc-lang]: ./haddock/recalc/src/Recalc.Language.html#line-397
  [core]: ./core.md
  [settings]: https://code.visualstudio.com/docs/editor/settings#_user-settings
