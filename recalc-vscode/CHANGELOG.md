# Revision history for recalc-vscode

## 0.0.8 -- 2025-02-10

* Initial version: create project structure for a custom editor.
* Add generic rpc implementation, client implementation and logging facilities.
* Integrate type-safe json-rpc communication between client and server.
* Add simple UI code using [Univer core/sheet-api][univer-sheet-api].
* Add test suite runner, package the extension with Nix.
* Implement json-rpc tests.
* Add client test: connect to server, send "open" message and check response.
* Implement file handling and basic tests for `.rc` files, update publish workflow
  and fix workflows.

<!-- References -->

  [univer-sheet-api]: https://docs.univer.ai/en-US/guides/sheets/features/core/sheet-api
