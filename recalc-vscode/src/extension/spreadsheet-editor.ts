import * as vscode from 'vscode';

import { readFileSync } from 'fs';
const nanoid = require("nanoid");

import { Message } from '../frontend/rpc';
import { Client, Params } from '../rpc/client';

type SheetDocument = {
	id: string,
	sheetOrder: [string, string][],
	sheets: {
		[sheetId: string]: {
			id: string,
			name: string,
		}
	}
};

class Spreadsheet extends vscode.Disposable implements vscode.CustomDocument {
  client: Client<SpreadsheetProtocol>;
  uri: vscode.Uri;
  initialData: SheetDocument;

	constructor(uri: vscode.Uri, client: Client<SpreadsheetProtocol>) {
    super(() => console.log(`Custom(${this.uri}).dispose()`));
    this.uri = uri;
    this.client = client;

		switch (uri.scheme) {
			case "file":
				this.initialData = this.openFile(uri);
				break;
			default:
				this.initialData = {
					id: btoa(uri.toString()),
					sheetOrder: [],
					sheets: {},
				};
				console.error(`cannot open file: unsupported uri-scheme '${uri.scheme}'`)
		}
  }

	private openFile(uri: vscode.Uri) {
		let data: SheetDocument;

		const path = uri.path;
		const src = JSON.parse(readFileSync(path).toString('utf-8'));

		if (src.sheetOrder !== undefined && src.sheets !== undefined) {
			const randomIds = new Map(
				Object.keys(src.sheets).map(k => [k, nanoid.nanoid()])
			);

			data = {
				id: btoa(uri.toString()),
				sheetOrder: src.sheetOrder.map((x: string) => [randomIds.get(x)!, x]),
				sheets: Object.fromEntries(
					Object.keys(src.sheets).map(
						x => [randomIds.get(x), { id: randomIds.get(x), name: x }],
					)
				),
			};
		} else {
			const defaultSheetId = nanoid.nanoid();
			data = {
				id: btoa(uri.toString()),
				sheetOrder: [[defaultSheetId, "Sheet 1"]],
				sheets: { [defaultSheetId]: { id: defaultSheetId, name: "Sheet 1" } },
			};
		}

		this.client.request("open", {uri: this.uri.toString(), sheetOrder: data.sheetOrder});

		return data;
	}
}

export class SpreadsheetEditorProvider implements vscode.CustomEditorProvider<Spreadsheet> {

	public static register(context: vscode.ExtensionContext, _config: vscode.WorkspaceConfiguration, client: Client<SpreadsheetProtocol>): vscode.Disposable {
		return vscode.window.registerCustomEditorProvider(
      SpreadsheetEditorProvider.viewType,
      new SpreadsheetEditorProvider(context, client)
    );
	}

	private static readonly viewType = 'recalc.editor';
  private extensionUri: vscode.Uri;
	private client: Client<SpreadsheetProtocol>;
  private webviews: WebviewCollection;

	constructor(private readonly context: vscode.ExtensionContext, client: Client<SpreadsheetProtocol>) {
    this.extensionUri = context.extensionUri;
    this.client = client;
    this.webviews = new WebviewCollection();
  }

  /* custom change events */
	private readonly _onDidChangeCustomDocument = new vscode.EventEmitter<vscode.CustomDocumentEditEvent<Spreadsheet>>();
	public onDidChangeCustomDocument = this._onDidChangeCustomDocument.event;

  /**
   * Called when our custom editor is opened.
   *
   * @param document that is being opened
   * @param webviewPanel the webview associated to the document
   * @param token cancellation token (eg. custom editor gets closed)
   */
  resolveCustomEditor(document: Spreadsheet, webviewPanel: vscode.WebviewPanel, _token: vscode.CancellationToken) {
    // keep track of webview
    this.webviews.add(document.uri, webviewPanel);

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

    // Set up RPC through client:

    // when the client receives a notification, forward it to all webviews associated with that uri
		// eslint-disable-next-line @typescript-eslint/no-explicit-any
		this.client.onNotification((method: string, params: any) => {
			console.log(`got notification(${method}, ${JSON.stringify(params)})`)
			switch (method) {
				case "setCells":
					// forward updated sheets to all webviews subscribed to uri ({uri: {sheet: {..}, ..}, ..})
					for (const [uri, sheets] of Object.entries(params)) {
						for (const webviewPanel of this.webviews.get(vscode.Uri.parse(uri))) {
							webviewPanel.webview.postMessage(sheets)
						}
					}
					break;
				default:
					console.warn(`unknown Notification: ${method} (params: ${JSON.stringify(params)})`)
			}
		});

    // when a webview sends a message, forward it to the server via client and pass back result
		webviewPanel.webview.onDidReceiveMessage(<M extends keyof SpreadsheetProtocol>(message: Message<M>) => {

			if (message.notification) console.warn(`message should be notification`);

			this.client.logger.log(`webview(${document.uri}) sent: ${JSON.stringify(message)}`);

      if (typeof message === "object"
			&& "method" in message && typeof message.method === "string"
			&& "params" in message && typeof message.params === "object"
			&& !("uri" in message.params))
			{
				const params = {
					...message.params,
					uri: document.uri.toString()
				} as Params<SpreadsheetProtocol, M>;

				this.client
					.request(message.method, params)
					.then(result => {
						this.client.logger.log(`webview(${document.uri}) got: ${result}`);
						webviewPanel.webview.postMessage(result);
					})
			} else {
        vscode.window.showErrorMessage(`A webview sent an invalid message: ${JSON.stringify(message)}`);
			}
		});

		// Make sure we get rid of the listener when our editor is closed.
		webviewPanel.onDidDispose(() => changeDocumentSubscription.dispose() );

    webviewPanel.webview.html = this.getHtmlForWebview(webviewPanel.webview, document);
  }

	/**
	 * Get the static html used for the editor webviews.
	 */
	private getHtmlForWebview(webview: vscode.Webview, document: Spreadsheet): string {

    const nonce = `nonce-${nanoid.nanoid(32)}`;

    const mkUri = (...pathSegments: string[]) =>
      webview.asWebviewUri(vscode.Uri.joinPath(this.extensionUri, "dist", ...pathSegments));

		const config = vscode.workspace.getConfiguration('spreadsheet-vscode');

    return /* html */`
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="UTF-8">
        <!--meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src ${webview.cspSource}; style-src ${webview.cspSource}; script-src 'nonce-${nonce}';"-->
        <meta name="viewport" content="width=device-width, height=device-height, initial-scale=1.0"/>
        <link rel="stylesheet" nonce="${nonce}" href="${mkUri("index.css")}">
				<style>
					html, body {
						margin: 0;
						padding: 0;

						height: 100%;
						width: 100%;
					}

					#app {
						height: 100%;
						width: 100%;
						overflow: hidden;
					}
				</style>
        <title>Spreadsheet</title>
      </head>
      <body>
				<div id="app"></div>
				<script nonce="${nonce}">
					const config = ${JSON.stringify(config)};
					const data = ${JSON.stringify(document.initialData)};
				</script>
        <script type="module" nonce="${nonce}" src="${mkUri("index.js")}"></script>
      </body>
      </html>`;
  }

  /**
   * Saves the custom document.
   *
   * @param document the custom document to save
   * @param token cancellation token
   */
  saveCustomDocument(_document: Spreadsheet, _token: vscode.CancellationToken): Thenable<void> {
    throw new Error('Method not implemented.');
  }

  /**
   * Saves the custom document to a new location.
   *
   * @param document the custom document to save
   * @param destination the destination URI to save the document
   * @param token cancellation token
   */
  saveCustomDocumentAs(_document: Spreadsheet, _destination: vscode.Uri, _token: vscode.CancellationToken): Thenable<void> {
    throw new Error('Method not implemented.');
  }

  /**
   * Reverts the custom document to its last saved state.
   *
   * @param document the custom document to revert
   * @param token cancellation token
   */
  revertCustomDocument(_document: Spreadsheet, _token: vscode.CancellationToken): Thenable<void> {
    throw new Error('Method not implemented.');
  }

  /**
   * Creates a backup of the custom document.
   *
   * @param document the custom document to back up
   * @param context context providing information about the backup
   * @param token cancellation token
   */
  backupCustomDocument(_document: Spreadsheet, _context: vscode.CustomDocumentBackupContext, _token: vscode.CancellationToken): Thenable<vscode.CustomDocumentBackup> {
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
  openCustomDocument(uri: vscode.Uri, _openContext: vscode.CustomDocumentOpenContext, _token: vscode.CancellationToken): Spreadsheet {
    return new Spreadsheet(uri, this.client);
  }
}

/**
 * Tracks all webviews.
 */
class WebviewCollection {

	private readonly _webviews = new Set<{
		readonly resource: string;
		readonly webviewPanel: vscode.WebviewPanel;
	}>();

	/**
	 * Get all known webviews for a given uri.
	 */
	public *get(uri: vscode.Uri): Iterable<vscode.WebviewPanel> {
		const key = uri.toString();
		for (const entry of this._webviews) {
			if (entry.resource === key) {
				yield entry.webviewPanel;
			}
		}
	}

	/**
	 * Add a new webview to the collection.
	 */
	public add(uri: vscode.Uri, webviewPanel: vscode.WebviewPanel) {
		const entry = { resource: uri.toString(), webviewPanel };
		this._webviews.add(entry);

		webviewPanel.onDidDispose(() => {
			this._webviews.delete(entry);
		});
	}
}
