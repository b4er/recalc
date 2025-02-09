/* eslint-disable  @typescript-eslint/no-explicit-any */

export enum Loglevel {
  Error = 1,
  Warning = 2,
  Info = 3,
  Debug = 4,
}

/** extends rpc.Logger (compatible) with data and flag (when set vscode will open a notification) */
export type IExtensionLogger = {
	error: (message: string, error?: Error, data?: any, showNotification?: boolean) => void;
	warn: (message: string, data?: any, showNotification?: boolean) => void;
	info: (message: string, data?: any, showNotification?: boolean) => void;
	log: (message: string, data?: any, showNotification?: boolean) => void;
	dispose?: () => void;
}

/** use this Logger for testing */
export const simpleLogger: IExtensionLogger = {
	error: (message: string, error?: Error, data?: any, _showNotification?: boolean) =>
		simpleOutput(console.error, Loglevel.Error, `${message}${error ? `(${error})` : ''}`, data),
	warn: (message: string, data?: any, _showNotification?: boolean) =>
		simpleOutput(console.warn, Loglevel.Warning, message, data),
	info: (message: string, data?: any, _showNotification?: boolean) =>
		simpleOutput(console.info, Loglevel.Info, message, data),
	log: (message: string, data?: any, _showNotification?: boolean) =>
		simpleOutput(console.log, Loglevel.Debug, message, data),
};

function simpleOutput(printer: (...data: any) => void, type: Loglevel, message: string, data?: any) {
	const level = Loglevel[type];
	printer(level, message)
	if (data !== null && data !== undefined) {
		printer(level, JSON.stringify(data, null, 2));
	}
}
