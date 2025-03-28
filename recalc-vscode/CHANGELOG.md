# Revision history for recalc-vscode

## 1.1.0 -- 2025-03-28

* Server-side implementation used by the TypeScript frontend, handling:
  * JSON-RPC communication for value ranges, sheet insertion, sheet removal,
    worksheet order, worksheet name, and sheet-defined functions (dummy
handler)
  * Cell diagnostics for parse errors, type errors, inferred types, and
    sheet-defined function editor.
* Added custom Univer plugin, RPC-controller, and webview logging
  functionality.
* Implemented sheet-defined function editor side panel (description,
  re-orderable inputs, and output).
* Full test suite runner, including JSON-RPC and end-to-end tests.
* Static builds and project packaging with Nix for `.vsix`.
* Improved UI for diagnostics, function editor, and added configuration options
  (`logLevel`).
* Enhanced RPC message tracing and cell diagnostics on hover.
* Configurable values for the extension:
  * `recalc-vscode.locale`: Sets the locale for the spreadsheet editor
    (options: `EN_US`, `ZH_CN`, default: `EN_US`).
  * `recalc-vscode.serverUri`: Specifies the URI for the spreadsheet server
    (resolved via `@cabal list-bin@` if empty).
  * `recalc-vscode.logLevel`: Controls the log verbosity
    (`debug`, `info`, `warning`, `error`, default: `info`).
  * `recalc-vscode.serverMaxRestartCount`: Defines the maximum attempts to
    restart the server (default: `5`).
