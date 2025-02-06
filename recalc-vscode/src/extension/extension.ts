import * as vscode from 'vscode';
import { CustomEditorProvider } from './customEditor';

export function activate(context: vscode.ExtensionContext) {
	// read vscode app-specific settings (as specified in package.json)
	const config = vscode.workspace.getConfiguration('recalc-vscode');

	// register the custom editor and pass it the client instance
	context.subscriptions.push(CustomEditorProvider.register(context, config))
}
