# Revision history for recalc

## 0.5.0.0 -- 2025-02-10

* Initial version: project skeleton for recalculation engine + test suite
* First implementation: minimal recalculation engine for generic language
  implementation based on "Build systems à la carte". Includes a simple
  example language as part of the test-suite.

  Add [`README.md`](./README.md) and document the project organisation a bit.
* Add Syntax for the term language (simple dependently typed lambda calculus).
* Add support for operators with precedences.
* Prepare TypeScript code generation.
* Add server implementation that is used as backend by the TypeScript frontend.
* Generates type-safe routing for TypeScript from Servant-like named protocols,
  implements a generic server based on the reactor pattern (single thread reads
  json-rpc messages and queues new jobs, multiple workers handle jobs).
* Send "ok" from , gracefully terminate when the client disconnects (by eof).
* Add more rpc routes for spreadsheet operations (set value ranges,
  sheet insertion, sheet removal, worksheet order, work sheet name).
