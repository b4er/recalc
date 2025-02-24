# Set up the Development Environment

Open a dev shell:

```bash
nix develop
```

## Running Tests

This will drop you into a shell with all necessary dependencies available where you can run
the test suite:

```bash
cabal test
npm --prefix recalc-vscode run tests
```

## Running the Web Extension in Development

In a dev shell open the current directory (eg. `codium .`), launch the extension
development host using (F5).

## Logging

When running the extension open the [Output view][vscode-output] and select the `recalc`
channel. You should see the logs there (you can set the **Log Level** in the Extension
Settings).

The Haskell backend uses *stdout* for JSON-RPC, but all output written to *stderr*
will be forwarded to the Output channel mentioned earlier.

When running the NPM tests manually, you can set the environment variable
`LOG_OUTPUT=true` and it will write logs during the tests to a logfile.

<!-- References: -->

  [vscode-output]: https://code.visualstudio.com/docs/getstarted/userinterface#_basic-layout
