import { WebviewApi } from 'vscode-webview';

export const vscode: WebviewApi<never> = acquireVsCodeApi();

export type Message<M extends keyof SpreadsheetProtocol> = {
  method: M;
  params: Omit<SpreadsheetProtocol[M]["params"], "uri">;
  notification?: boolean,
}

export function postMessage<M extends keyof SpreadsheetProtocol>(message: Message<M>): void {
  return vscode.postMessage(message);
}

// FIXME: handle messages
window.addEventListener('message',
  message => console.log(`received message: ${message}`)
);
