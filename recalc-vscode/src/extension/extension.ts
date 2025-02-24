import * as rpc from 'vscode-jsonrpc/node';
import * as vscode from 'vscode';

import { ChildProcessWithoutNullStreams, execSync, spawn } from 'child_process';

import { Client, MessageTransports } from '../rpc/client';
import { SpreadsheetEditorProvider } from './spreadsheet-editor';
import { existsSync } from 'fs';
import { ExtensionLogger } from './logging';
import { readLoglevel } from '../rpc/logging';

export function activate(context: vscode.ExtensionContext) {
	// read vscode app-specific settings (as specified in package.json)
	const config = vscode.workspace.getConfiguration('recalc-vscode');

	// JSON-RPC client implementation
	const client = new class extends Client<SpreadsheetProtocol> {
		public readonly name = "recalc";

		private process?: ChildProcessWithoutNullStreams;

		constructor() {
			super(config.serverMaxRestartCount, new ExtensionLogger("recalc", readLoglevel(config.logLevel)));
		}

		protected async createMessageTransports(encoding: "utf-8" | "ascii"): Promise<MessageTransports> {

			// vsix is bundled with the -server here:
			const _binName = `${this.name}-server`;
			const defaultBinPath = `${context.extensionPath}/bin/${_binName}`;

			// guess the location for the -server
			const binPath = config.serverPath
				|| existsSync(defaultBinPath)
						? defaultBinPath
						: execSync(`cabal list-bin ${_binName}`).toString('utf-8').trim();

			// spawn and hook up message transports
			this.logger.info(`Starting: ${binPath}`, undefined, true);
			this.process = spawn("sh", ["-c", binPath]);

			if (!this.process || !this.process.stdout || !this.process.stderr) {
				throw new Error('Failed to start process.');
			}

			const reader = new rpc.StreamMessageReader(this.process.stdout, encoding);
			const writer = new rpc.StreamMessageWriter(this.process.stdin, encoding);

			const binName = binPath.split("/").at(-1);
			this.process.stderr.on('data', data =>
				this.logger.log(`(${binName}) ${data.toString().trim()}`)
			);

			return {reader: reader, writer: writer}
		}

		override dispose(): void {
			super.dispose();
			this.logger.dispose?.();
			if (this.process?.pid !== undefined) {
				this.process.kill()
			}
		}
	};

	// register the custom editor and pass it the client instance
	context.subscriptions.push(SpreadsheetEditorProvider.register(context, config, client))
}
