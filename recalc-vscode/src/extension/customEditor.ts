import * as vscode from 'vscode';
import { nanoid } from "nanoid"


class Custom extends vscode.Disposable implements vscode.CustomDocument {
  uri: vscode.Uri;

	constructor(uri: vscode.Uri) {
    super(() => console.log(`Custom(${this.uri}).dispose()`));
    this.uri = uri;
  }
}

export class CustomEditorProvider implements vscode.CustomEditorProvider<Custom> {

	public static register(context: vscode.ExtensionContext, _config: vscode.WorkspaceConfiguration): vscode.Disposable {
		const provider = new CustomEditorProvider(context);
		const providerRegistration = vscode.window.registerCustomEditorProvider(CustomEditorProvider.viewType, provider);
		return providerRegistration;
	}

	private static readonly viewType = 'recalc.editor';
  private extensionUri: vscode.Uri;

	constructor(private readonly context: vscode.ExtensionContext) {
    this.extensionUri = context.extensionUri;
  }

  /* custom change events */
	private readonly _onDidChangeCustomDocument = new vscode.EventEmitter<vscode.CustomDocumentEditEvent<Custom>>();
	public onDidChangeCustomDocument = this._onDidChangeCustomDocument.event;

  /**
   * Called when our custom editor is opened.
   *
   * @param document that is being opened
   * @param webviewPanel the webview associated to the document
   * @param token cancellation token (eg. custom editor gets closed)
   */
  resolveCustomEditor(document: Custom, webviewPanel: vscode.WebviewPanel, _token: vscode.CancellationToken) {
		// setup initial content for the webview
		webviewPanel.webview.options = {
			enableScripts: true,
			localResourceRoots: [vscode.Uri.joinPath(this.extensionUri, "dist")]
		};

		// The text document acts as our model, so we have to sync change in the document to our
		// editor and sync changes in the editor back to the document.
		function updateWebview() {
			console.log(`updateWebview()`)
		}

		// Hook up event handlers so that we can synchronize the webview with the text document.
		const changeDocumentSubscription = vscode.workspace.onDidChangeTextDocument(e => {
			if (e.document.uri.toString() === document.uri.toString()) {
				updateWebview();
			}
		}).dispose();

		// Make sure we get rid of the listener when our editor is closed.
		webviewPanel.onDidDispose(() => changeDocumentSubscription.dispose() );

    webviewPanel.webview.html = this.getHtmlForWebview(webviewPanel.webview);
  }

	/**
	 * Get the static html used for the editor webviews.
	 */
	private getHtmlForWebview(webview: vscode.Webview): string {

    const nonce = `nonce-${nanoid(32)}`;

    const mkUri = (...pathSegments: string[]) =>
      webview.asWebviewUri(vscode.Uri.joinPath(this.extensionUri, "dist", ...pathSegments));

    return /* html */`
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="UTF-8">
        <meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src ${webview.cspSource}; style-src ${webview.cspSource}; script-src 'nonce-${nonce}';">
        <meta name="viewport" content="width=device-width, height=device-height, initial-scale=1.0"/>
        <link rel="stylesheet" nonce="${nonce}" href="${mkUri("index.css")}">
        <title>Spreadsheet</title>
      </head>
      <body>
        <h1>Hello, HTML!</h1>
        <script nonce="${nonce}" src="${mkUri("index.js")}"></script>
      </body>
      </html>`;
  }

  /**
   * Saves the custom document.
   *
   * @param document the custom document to save
   * @param token cancellation token
   */
  saveCustomDocument(_document: Custom, _token: vscode.CancellationToken): Thenable<void> {
    throw new Error('Method not implemented.');
  }

  /**
   * Saves the custom document to a new location.
   *
   * @param document the custom document to save
   * @param destination the destination URI to save the document
   * @param token cancellation token
   */
  saveCustomDocumentAs(_document: Custom, _destination: vscode.Uri, _token: vscode.CancellationToken): Thenable<void> {
    throw new Error('Method not implemented.');
  }

  /**
   * Reverts the custom document to its last saved state.
   *
   * @param document the custom document to revert
   * @param token cancellation token
   */
  revertCustomDocument(_document: Custom, _token: vscode.CancellationToken): Thenable<void> {
    throw new Error('Method not implemented.');
  }

  /**
   * Creates a backup of the custom document.
   *
   * @param document the custom document to back up
   * @param context context providing information about the backup
   * @param token cancellation token
   */
  backupCustomDocument(_document: Custom, _context: vscode.CustomDocumentBackupContext, _token: vscode.CancellationToken): Thenable<vscode.CustomDocumentBackup> {
    throw new Error('Method not implemented.');
  }

  /**
   * Opens the custom document from the specified URI.
   *
   * @param uri the URI of the custom document to open
   * @param openContext context providing information about how the document is being opened
   * @param token cancellation token
   * @returns the custom document or a promise that resolves to it
   */
  openCustomDocument(uri: vscode.Uri, _openContext: vscode.CustomDocumentOpenContext, _token: vscode.CancellationToken): Custom {
    return new Custom(uri);
  }
}
