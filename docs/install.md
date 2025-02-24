# Installing the Web Extension

The easiest is installing the extension from the Visual Studio Marketplace:

[![Visual Studio Marketplace](https://vsmarketplacebadges.dev/version/b4er.recalc-vscode.svg)][recalc-vscode]

## Installing from Source

You can [build the extension][build] from source and then install it with:

```bash
# set your editor (eg. EDITOR=codium)
EDITOR=code
version="$(jq -r .version < recalc-vscode/package.json)"

$EDITOR --install-extension "result/recalc-vscode-${version}.vsix"
```

The Webview Extension should automatically start when opening a file
ending with `.rc`.

<!-- References -->

  [build]: ./build.md
  [recalc-vscode]: https://marketplace.visualstudio.com/items/b4er.recalc-vscode
