import * as rpc from 'vscode-jsonrpc/node';
import * as vscode from 'vscode';

import { ChildProcessWithoutNullStreams, execSync, spawn } from 'child_process';

import { Client, MessageTransports } from '../rpc/client';
import { SpreadsheetEditorProvider } from './customEditor';

export function activate(context: vscode.ExtensionContext) {
	// read vscode app-specific settings (as specified in package.json)
	const config = vscode.workspace.getConfiguration('recalc-vscode');

	// json-rpc client implementation
	const client = new class extends Client<SpreadsheetProtocol> {
		public name = "recalc";

		private process?: ChildProcessWithoutNullStreams;

		protected async createMessageTransports(encoding: "utf-8" | "ascii"): Promise<MessageTransports> {

			const binPath = config.serverPath || execSync("cabal list-bin server").toString('utf-8').trim();
			this.log(`starting: ${binPath}`);
			this.process = spawn("sh", ["-c", binPath]);

			if (!this.process || !this.process.stdout || !this.process.stderr) {
				throw new Error('Failed to start process.');
			}

			const reader = new rpc.StreamMessageReader(this.process.stdout, encoding);
			const writer = new rpc.StreamMessageWriter(this.process.stdin, encoding);

			this.process.stderr.on('data', data => this.info(`server-stderr: ${data.toString().trim()}`));

			return {reader: reader, writer: writer}
		}

		override dispose(): void {
			super.dispose();
			if (this.process?.pid !== undefined) {
				this.process.kill()
			}
		}
	}(config.serverMaxRestartCount);

	// register the custom editor and pass it the client instance
	context.subscriptions.push(SpreadsheetEditorProvider.register(context, config, client))
}
