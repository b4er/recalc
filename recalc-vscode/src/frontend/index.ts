import { WebviewApi } from 'vscode-webview';
import { Message } from '../extension/customEditor';

const vscode: WebviewApi<never> = acquireVsCodeApi();

export function postMessage<M extends keyof SpreadsheetProtocol>(message: Message<M>): void {
  return vscode.postMessage(message);
}

window.addEventListener('message',
  message => console.log(`received message: ${message}`)
);

postMessage({method: "close", params: {}});
