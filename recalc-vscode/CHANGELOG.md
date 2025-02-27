# Revision history for recalc-vscode

## 0.0.21 -- 2025-02-26

* Initial version: create project structure for a custom editor.
* Add generic rpc implementation, client implementation and logging facilities.
* Integrate type-safe JSON-RPC communication between client and server.
* Add simple UI code using [Univer core/sheet-api][univer-sheet-api].
* Add test suite runner, package the extension with Nix.
* Implement JSON-RPC tests.
* Add client test: connect to server, send "open" message and check response.
* Implement file handling and basic tests for `.rc` files, update publish workflow
  and fix workflows.
* Add custom Univer plugin, add RPC-controller, customize UI a bit and add webview
  logging functionality.
* Add JSON-RPC server-side error handling, finish the RPC-controller (handling of
  all messages).
* Add hover controller and cell diagnostics UI (highlight cells with parse-errors,
  type errors or eval errors), merged cell diagnostics not supported.
* Implement description service (single hard-coded description), UI fixes.
* Bump version for publishing the statically built version.
* Add menu UI for sheet-defined functions.
* Implement the sheet-defined function editor side panel: accepts description,
  re-orderable inputs (name+range selection), and output (range selection).

  The [`RangeSelector`](./src/frontend/views/components/RangeSelector.tsx) does not
  allow range selection due to issues with Univer's `RefSelectionsRenderService`,
  and the built-in `RangeSelector` causes other issues.
* Fix configuration and add `logLevel` setting, defaults to Info, add call for
  defining the function.
* Trace rpc messages, improve cell diagnostics when hovering.
* Implement simple end-to-end tests (resolve references, test type error
  diagnostics etc.)
* Extend cell diagnostics to show but not highlight info level diagnostics
  (for type annotations).
* Implement reference inference (infers as them [m,n] tensors), disable built-in
  spilling.

<!-- References -->

  [univer-sheet-api]: https://docs.univer.ai/en-US/guides/sheets/features/core/sheet-api
