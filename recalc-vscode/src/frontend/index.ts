import { WebviewApi } from 'vscode-webview';

const vscode: WebviewApi<never> = acquireVsCodeApi();

window.addEventListener('message',
  (x: string) =>
    console.log(`GOT MESSAGE: ${x}`)
);

vscode.postMessage({method: "hi", params: {origin: "frontend", destination: "haskell"}});
