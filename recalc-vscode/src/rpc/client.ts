import * as rpc from 'vscode-jsonrpc/node';

import { IExtensionLogger, Loglevel, simpleLogger } from './logging';

export interface MessageTransports {
	reader: rpc.MessageReader;
	writer: rpc.MessageWriter;
}

export abstract class BaseClient {
  public abstract name: string;
	protected abstract createMessageTransports(encoding: "utf-8" | "ascii"): Promise<MessageTransports>;

  private _state: BaseClient.State;
  private _connection?: rpc.MessageConnection;
  private _errorHandler: ErrorHandler;
  private _onStart?: Promise<void>;
  private _onStop?: Promise<void>;

  private extensionLogger: IExtensionLogger;

  constructor(serverMaxRestartCount: number, logger?: IExtensionLogger) {
    this._state = BaseClient.State.Initial;
    this._errorHandler = new ErrorHandler(this, serverMaxRestartCount);
		this.extensionLogger = logger ? logger : simpleLogger;
  }

  public get logger(): IExtensionLogger {
		return this.extensionLogger;
	}

  public async start(): Promise<void> {
		if (this._state === BaseClient.State.Stopping) {
			throw new Error(`Client is currently stopping. Can only restart a full stopped client`);
		}

		if (this._onStart !== undefined) {
			return this._onStart;
		}
		const [promise, resolve, reject] = this.createOnStartPromise();
		this._onStart = promise;

    this._state = BaseClient.State.Starting;

    try {
      const connection = await this.createConnection();

			connection.listen();
			this._state = BaseClient.State.Running;
			this.extensionLogger.log("RPC server started and connection established.")

      await this.initialize(connection);
			this.extensionLogger.log("Connection successfully initialized.")

      resolve();
    } catch (error) {
			this._state = BaseClient.State.StartFailed;
			this.extensionLogger.error(`${this.name} client: couldn't create connection to ${this.name} server`, error as Error);
			reject(error);
    }

    return this._onStart;
  }

	protected async activeConnection(): Promise<rpc.MessageConnection> {
		if (this._state === BaseClient.State.StartFailed) {
			throw new Error(`Previous start failed. Can't restart server.`);
		}

		await this.start();

		if (this._state !== BaseClient.State.Running || this._connection === undefined) {
			throw new Error(`Starting server failed`);
		}

		return this._connection;
	}

  private async createConnection(): Promise<rpc.MessageConnection> {
		const transports = await this.createMessageTransports('utf-8');
    this._connection = rpc.createMessageConnection(transports.reader, transports.writer, this.logger);

    this._connection.onError((args: [Error, rpc.Message | undefined, number | undefined]) =>
      this.handleConnectionError(args[0], args[1], args[2]).catch(
        error => this.extensionLogger.error("Could not handle connection error", error)
      )
    );

    this._connection.onClose(() =>
			this.handleConnectionClosed().catch(
        error => this.extensionLogger.error(`Handling connection close failed`, error)
      )
		);

    return this._connection;
	}

	/** by default no initialization */
	protected async initialize(_connection: rpc.MessageConnection): Promise<void> { }

	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	public onNotification(notificationHandler: (method: string, params: any) => void) {
		this._connection?.onNotification(notificationHandler)
	}

	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	private createOnStartPromise(): [ Promise<void>, () => void, (error:any) => void] {
		let resolve!: () => void;
		// eslint-disable-next-line @typescript-eslint/no-explicit-any
		let reject!: (error: any) => void;
		const promise: Promise<void> = new Promise((_resolve, _reject) => {
			resolve = _resolve;
			reject = _reject;
		});
		return [promise, resolve, reject];
  }

	public stop(timeout: number = 2000): Promise<void> {
		// Wait 2 seconds on stop
		return this.shutdown(ShutdownMode.DoNotRestart, timeout);
	}

	protected async shutdown(mode: ShutdownMode, timeout: number = 2000): Promise<void> {
		// If the client is stopped or in its initial state return.
		if (this._state === BaseClient.State.Stopped || this._state === BaseClient.State.Initial) {
			return;
		}

		// If we are stopping the client and have a stop promise return it.
		if (this._state === BaseClient.State.Stopping) {
			if (this._onStop !== undefined) {
				return this._onStop;
			} else {
				throw new Error(`Client is stopping but no stop promise available.`);
			}
		}

    const connection = this._connection;

		// We can't stop a client that is not running (e.g. has no connection). Especially not
		// on that us starting since it can't be correctly synchronized.
		if (connection === undefined || this._state !== BaseClient.State.Running) {
			throw new Error(`Client is not running and can't be stopped. (current state: ${this._state})`);
		}

    this._state = BaseClient.State.Stopping;

		this.cleanUp(mode);

		const tp = new Promise<undefined>(c => { rpc.RAL().timer.setTimeout(c, timeout); });
		const shutdown = (async (connection) => {
      // FIXME: app level tear down..
      connection.dispose();
      return connection;
		})(connection);

		return this._onStop = Promise.race([tp, shutdown]).then((connection) => {
			// The connection won the race with the timeout.
			if (connection !== undefined) {
				connection.end();
				connection.dispose();
			} else {
        const err = new Error(`Stopping the server timed out`);
				this.extensionLogger.error(`Stopping the server (timed out)`, undefined, false);
				throw err;
			}
		}, (error) => {
			this.extensionLogger.error(`Stopping server failed`, error, false);
			throw error;
		}).finally(() => {
			if (mode === ShutdownMode.DoNotRestart) {
        this.extensionLogger.dispose?.();
      }

			this._onStart = undefined;
			this._onStop = undefined;
			this._connection = undefined;
			this._state = BaseClient.State.Stopped;
		});
  }

	protected sendRequest<R>(...args: Parameters<rpc.MessageConnection['sendRequest']>): Promise<R> {
		if (this._state === BaseClient.State.StartFailed || this._state === BaseClient.State.Stopping || this._state === BaseClient.State.Stopped) {
			return Promise.reject(new rpc.ResponseError(rpc.ErrorCodes.ConnectionInactive, `Client is has stopped working or is stopping.`))
		}

		return this.activeConnection().then(connection => connection.sendRequest<R>(...args))
	}

	protected sendNotification(notification: string) {
		if (this._state === BaseClient.State.StartFailed || this._state === BaseClient.State.Stopping || this._state === BaseClient.State.Stopped) {
			return Promise.reject(new rpc.ResponseError(rpc.ErrorCodes.ConnectionInactive, `Client is has stopped working or is stopping.`))
		}

		return this.activeConnection().then(connection => connection.sendNotification(notification))
	}

  public dispose() {
    this.cleanUp(ShutdownMode.DoNotRestart);
  }

  private cleanUp(_mode: ShutdownMode) {
    // FIXME: clean up things
  }

	private async handleConnectionError(error: Error, message: rpc.Message | undefined, count: number | undefined): Promise<void> {
		const handlerResult: ErrorHandlerResult = await this._errorHandler.error(error, message, count);
		if (handlerResult.action === ErrorAction.Shutdown) {
			this.extensionLogger.error(handlerResult.message ?? `Client ${this.name}: connection to server is erroring.\n${error.message}\nShutting down server.`, error, handlerResult.handled !== true);
			this.stop().catch((error: Error) => {
				this.extensionLogger.error("Stopping server failed", error, false);
			});
		} else {
			this.extensionLogger.error(handlerResult.message ??
				`Client ${this.name}: connection to server is erroring.\n${error.message}`, error, handlerResult.handled !== true);
		}
	}

  private async handleConnectionClosed(): Promise<void> {
		// Check whether this is a normal shutdown in progress or the client stopped normally.
		if (this._state === BaseClient.State.Stopped) {
			return;
		}

		try {
			if (this._connection !== undefined) {
				this._connection.dispose();
			}
		} finally { /* disposing a connection could fail if error cases. */ }

		let handlerResult: CloseHandlerResult = { action: ShutdownMode.DoNotRestart };
		if (this._state !== BaseClient.State.Stopping) {
			try {
        handlerResult = await this._errorHandler.closed();
			} finally { /* ignore errors from error handlers */ }
		}

    this._connection = undefined;

		if (handlerResult.action === ShutdownMode.DoNotRestart) {
			this.extensionLogger.error(handlerResult.message ?? 'Connection to server got closed. Server will not be restarted.', undefined, handlerResult.handled !== true);
			this.cleanUp(ShutdownMode.DoNotRestart);
			if (this._state === BaseClient.State.Starting) {
				this._state = BaseClient.State.StartFailed;
			} else {
				this._state = BaseClient.State.Stopped;
			}
			this._onStop = Promise.resolve();
			this._onStart = undefined;
		} else if (handlerResult.action === ShutdownMode.Restart) {
			this.extensionLogger.info(handlerResult.message ?? 'Connection to server got closed. Server will restart.', undefined, !handlerResult.handled);
			this.cleanUp(ShutdownMode.Restart);
			this._state = BaseClient.State.Initial;
			this._onStop = Promise.resolve();
			this._onStart = undefined;
			this.start().catch((error) => this.extensionLogger.error(`Restarting server failed`, error, true));
		}
  }

}

// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace BaseClient {
  export enum State {
    Initial = 'initial',
    Starting = 'starting',
    StartFailed = 'failed',
    Running = 'running',
    Stopping = 'stopping',
    Stopped = 'stopped'
  }
}

export type Params<Protocol, M extends keyof Protocol> =
	Protocol[M] extends { params: infer P } ? P : never;

/**
 * somehow TypeScript is not able to narrow the params type based on the
 * method, so we use any for onNotification..
 *
 * type NotificationHandler<Protocol> = {
 *   [M in keyof Protocol & string]: (
 *     method: M,
 *     params: Protocol[M] extends { params: infer P } ? P : never
 *   ) => void;
 * };
 */

export abstract class Client<Protocol> extends BaseClient {
	notify(method: string): Promise<void> {
		return this.sendNotification(method);
	}

	request<M extends keyof Protocol>(
    method: M,
    params: Protocol[M] extends { params: infer T } ? T : never
  ): Promise<Protocol[M] extends { result: infer R } ? R : never>
	{
		return this.sendRequest(method as string, params)
	}
}


export enum ShutdownMode {
	/**
	 * Don't restart the server. The connection stays closed.
	 */
	DoNotRestart = 1,

	/**
	 * Restart the server.
	 */
	Restart = 2,
}

export type CloseHandlerResult = {
	/**
	 * The action to take.
	 */
	action: ShutdownMode;

	/**
	 * An optional message to be presented to the user.
	 */
	message?: string;

	/**
	 * If set to true the client assumes that the corresponding
	 * close handler has presented an appropriate message to the
	 * user and the message will only be log to the client's
	 * output channel.
	 */
	handled?: boolean;
};

/**
 * An action to be performed when the connection encountered an error
 */
enum ErrorAction {
  /**
   * Continue running the server.
   */
  Continue = 1,

  /**
   * Shutdown the server.
   */
  Shutdown = 2
}

type ErrorHandlerResult = {
  /**
   * The action to take.
   */
  action: ErrorAction;

  /**
   * An optional message to be presented to the user.
   */
  message?: string;

  /**
   * If set to true the client assumes that the corresponding
   * error handler has presented an appropriate message to the
   * user and the message will only be log to the client's
   * output channel.
   */
  handled?: boolean;
}

class ErrorHandler {
	private readonly restarts: number[];

	constructor(private client: BaseClient, private maxRestartCount: number) {
    if (maxRestartCount !== undefined && maxRestartCount < 0) {
      throw new Error(`Invalid maxRestartCount: ${maxRestartCount}`);
    }

		this.restarts = [];
	}

	public error(_error: Error, _message: rpc.Message | undefined, count: number | undefined): ErrorHandlerResult {
		if (count && count <= 3) {
			return { action: ErrorAction.Continue };
		}
		return { action: ErrorAction.Shutdown };
	}

	public closed(): CloseHandlerResult {
		this.restarts.push(Date.now());
		if (this.restarts.length <= this.maxRestartCount) {
			return { action: ShutdownMode.Restart };
		} else {
			const diff = this.restarts[this.restarts.length - 1] - this.restarts[0];
			if (diff <= 3 * 60 * 1000) {
				return { action: ShutdownMode.DoNotRestart, message: `The ${this.client.name} server crashed ${this.maxRestartCount+1} times in the last 3 minutes. The server will not be restarted. See the output for more information.` };
			} else {
				this.restarts.shift();

				return { action: ShutdownMode.Restart };
			}
		}
	}
}
