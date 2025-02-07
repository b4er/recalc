/* eslint @typescript-eslint/no-namespace: 0 */

export namespace Loglevel {
	/**
	 * An error message.
	 */
	export const Error = 1;
	/**
	 * A warning message.
	 */
	export const Warning = 2;
	/**
	 * An information message.
	 */
	export const Info = 3;
	/**
	 * A log message.
	 */
	export const Log = 4;
	/**
	 * A debug message.
	 *
	 */
	export const Debug = 5;
}

export type Loglevel = 1 | 2 | 3 | 4 | 5;
