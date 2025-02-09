import * as vscode from 'vscode';

import { Loglevel } from '../rpc/logging';

export enum RevealOutputChannelOn {
	Error,
	Warn,
	Info,
	Debug,
}

export class ExtensionLogger {
  private outputChannel: vscode.OutputChannel;

  constructor(name: string) {
    this.outputChannel = vscode.window.createOutputChannel(name);
  }

  private appendLog(level: string, message: string) {
    this.outputChannel.append(`[${level}] ${message.trim()}\n`);
  }

	// eslint-disable-next-line @typescript-eslint/no-explicit-any
  error(message: string, error?: Error, data?: any, showNotification: boolean = true) {
    this.logOutputMessage(Loglevel.Error, RevealOutputChannelOn.Error, `${message}${error ? `(${error})` : ''}`, data, showNotification);
  }

	// eslint-disable-next-line @typescript-eslint/no-explicit-any
  warn(message: string, data?: any, showNotification: boolean = true): void {
    this.logOutputMessage(Loglevel.Warning, RevealOutputChannelOn.Warn, message, data, showNotification);
  }

	// eslint-disable-next-line @typescript-eslint/no-explicit-any
  info(message: string, data?: any, showNotification: boolean = true): void {
    this.logOutputMessage(Loglevel.Info, RevealOutputChannelOn.Info, message, data, showNotification);
  }

	// eslint-disable-next-line @typescript-eslint/no-explicit-any
  log(message: string, data?: any, showNotification: boolean = true): void {
    this.logOutputMessage(Loglevel.Debug, RevealOutputChannelOn.Debug, message, data, showNotification);
  }

	// eslint-disable-next-line @typescript-eslint/no-explicit-any
  private logOutputMessage(type: Loglevel, _reveal: RevealOutputChannelOn, message: string, data: any | undefined, showNotification: boolean): void {
		const level = Loglevel[type];
    this.appendLog(level, message);
    if (data !== null && data !== undefined) {
      this.appendLog(level, JSON.stringify(data, null, 2));
    }

    if (showNotification) {
      this.showNotificationMessage(type, message);
    }
  }

  private showNotificationMessage(type: Loglevel, message: string) {
    const messageFunc =
      type === Loglevel.Error
        ? vscode.window.showErrorMessage
        : type === Loglevel.Warning
        ? vscode.window.showWarningMessage
        : vscode.window.showInformationMessage;

    void messageFunc(message, 'Go to output').then((selection) => {
      if (selection !== undefined) {
        this.outputChannel.show(true);
      }
    });
  }

	public dispose() {
		this.outputChannel.dispose();
	}
}
