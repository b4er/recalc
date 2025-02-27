import assert from "node:assert";
import { ChildProcessWithoutNullStreams, execSync, spawn } from "node:child_process";
import * as fs from "node:fs";
import 'mocha';
import * as rpc from 'vscode-jsonrpc/node';

import { Client, MessageTransports, Params } from "../../rpc/client";
import { logger } from "../logging";
import { Semaphore } from "./semaphore";

const NAME = "test";

// sort by key when object (just for printing - makes deciphering errors much easier)
function json(value: unknown) {
  if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
    const keys = new Set<string>();
    JSON.stringify(value, (key, value) => (keys.add(key), value));
    return JSON.stringify(value, Array.from(keys).sort());
  }

  return JSON.stringify(value);
}

// deep equality for objects, if expected is `"*"` and `actual` is a string always passes
function deepEqualWithWildcard(expected: unknown, actual: unknown, message?: string) {
  if (expected === "*" && typeof actual === "string")
    return;

  if (typeof actual === "object" && typeof expected === "object" && actual !== null && expected !== null) {
    assert.strictEqual(Array.isArray(expected), Array.isArray(actual), message);

    for (const key of Object.keys(expected)) {
      deepEqualWithWildcard((expected as Record<string, unknown>)[key], (actual as Record<string, unknown>)[key], message);
    }
  } else {
    assert.deepStrictEqual(actual, expected, message);
  }
}

/**
 * Pretty much the same way we extend `Client<SpreadsheetProtocol>` in the
 * extension's `activate` function, except a different logger and config values.
 */
class TestClient extends Client<SpreadsheetProtocol> {
  public name = NAME;

  public process?: ChildProcessWithoutNullStreams;

  constructor() { super(1, logger); }

  protected createMessageTransports(encoding: "utf-8" | "ascii"): Promise<MessageTransports> {
    const _binName = "recalc-server";
    const binPath = fs.existsSync(`bin/recalc-server`)
      ? `bin/${_binName}`
      : execSync(`cd .. && cabal list-bin ${_binName}`).toString('utf-8').trim()

    // spawn and hook up message transports
    this.logger.info(`Starting: ${binPath}`);
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

    return Promise.resolve({reader: reader, writer: writer})
  }
};

/* End-to-end tests, spawn a recalc-server using `TestClient` and send some requests: */

describe('testClient (end-to-end tests)', function () {
  this.timeout(5000);

  it('should successfully send a "open" request and receive a response', async () => {
    const testClient = new TestClient();

    logger.info(`>>> starting TestClient`)
    await testClient.start();

    logger.info(`>>> send open request to TestClient`)
    const result = await testClient.request("open", {uri: "test://file.rc", sheetOrder: ["Sheet 1"]})

    assert.deepEqual(result, []);

    logger.info(`>>> shutting down TestClient`)
    await testClient.stop();
    await new Promise<void>((resolve, reject) => {
      testClient.process!.on('exit', (_code) => resolve());
      testClient.process!.on('error', reject);
    });
  });

  it('should successfully perform some simple cell operations using references', async () => {
    const uri = "test://file.rc";
    const sheetId = "Sheet 1";

    const testClient = new TestClient();

    logger.info(`>>> starting TestClient`)
    await testClient.start();

    logger.info(`>>> send open request to TestClient`)
    const result = await testClient.request("open", {uri, sheetOrder: [sheetId]})
    logger.log(`result: ${json(result)}`)
    assert.deepEqual(result, []);

    // need to signal and await to make sure the notification handler fires everytime,
    // collect the errors and raise at the end
    let errors: Error[] = [];

    // helper function asserts that when sending a {method,params} we get back the expected reply
    async function assertReply<M extends keyof SpreadsheetProtocol>(
      method: M, params: Params<SpreadsheetProtocol, M>,
      expectedMethod: string,
      expectedParams: unknown,
    ) {
      const sem = new Semaphore();
      logger.log(`>>> ${method}: ${json(params)}`);

      testClient.onNotification((rmethod, rparams) => {
        logger.log(`<<< ${rmethod}: ${json(rparams)}`);
        try {
          const message = `when sending\n>>> ${method}: ${JSON.stringify(params)}\ngot\n<<< ${rmethod}: ${json(rparams)}\nbut expected\n!!! ${expectedMethod}: ${json(expectedParams)}`;

          assert.strictEqual(expectedMethod, rmethod, message);
          deepEqualWithWildcard(expectedParams, rparams, message);
        } catch (err) {
          errors.push(err as Error);
        } finally {
          sem.signal();
        }
      });

      await testClient.request(method, params);
      await sem.wait();
    }

    // single Cell at `(i,j)` is set to `{v}`
    const Cell = (i: number, j: number,v: string, custom?: unknown) =>
      ({[uri]:{[sheetId]:[[[i,j],{v, ...(custom !== undefined ? {custom} : {})}]]}});

    // set `A1` to `=1` should give `1`
    await assertReply("setRangeValues", {
      uri, sheetId, cells: {0:{0:{f:"=1"}}},
    },
      "setCells", Cell(0,0, "1")
    );

    // set `A2` to `2` should give `2`
    await assertReply("setRangeValues", {
      uri, sheetId, cells: {1:{0:{v:"2"}}},
    },
      "setCells", Cell(1,0, "2")
    );

    // set `B1` to `=not(False)` should give `True`
    await assertReply("setRangeValues", {
      uri, sheetId, cells: {0:{1:{f:"=not(False)",si:null,v:null,p:null}}}
    },
      "setCells", Cell(0,1, "True")
    );

    // set `C1` to `=and(B1,True)` should give `True`
    await assertReply("setRangeValues", {
      uri, sheetId, cells: {0:{2:{f:"=and(B1, True)"}}}
    },
      "setCells", Cell(0,2, "True")
    );

    // set `C2` to `=A2` should give `2`
    await assertReply("setRangeValues", {
      uri, sheetId, cells: {1:{2:{f:"=A2"}}}
    },
      "setCells", Cell(1,2, "2")
    );

    // set `A2` to 3 should give 3; update C2
    await assertReply("setRangeValues", {
      uri, sheetId, cells: {1:{0:{v:"3"}}},
    },
      "setCells", {[uri]: {[sheetId]: [
          [[1,0],{v: "3"}],
          [[1,2],{v: "3"}],
      ]}}
    );

    // set `C2` to =and(B1,A1) should give error
    await assertReply("setRangeValues", {
      uri, sheetId, cells: {1:{2:{f:"=and(B1,A1 )"}}}
    },
      "setCells", Cell(1,2, "#error", {
        errors: [{
          title: "Type Mismatch",
          message: "*",
        }],
        warnings: [],
      })
    );

    // matrix tests

    // set `M1:M2` to 1,2
    await assertReply("setRangeValues", {
      uri, sheetId, cells: {0:{12:{v:"1"}},1:{12:{v:"2"}}},
    },
      "setCells", {[uri]: {[sheetId]: [
          [[0,12],{v: "1"}],
          [[1,12],{v: "2"}],
      ]}}
    );

    // set `N1` to `M1:M2` should give `⟨2,1⟩ [1, 2]`
    await assertReply("setRangeValues", {
      uri, sheetId, cells: {0:{13:{f:"=M1:M2"}}},
    },
      "setCells", {[uri]: {[sheetId]: [
          [[0,13],{"v":"[1, 2]", custom: {errors:[], warnings: [], info: [{message:"*"}]}}],
      ]}}
    );

    const refError = {"custom":{"errors":[{"message":"#REF","title":"Invalid Reference"}],"info":[],"warnings":[]},"v":"#error"};

    // set `N2` to `M2:M3` should give #error (ref)
    await assertReply("setRangeValues", {
      uri, sheetId, cells: {1:{13:{f:"=M2:M3"}}},
    },
      "setCells", {[uri]: {[sheetId]: [
          [[1,13],refError],
      ]}}
    );

    // when deleting M1, the reference in N1 (referring to M1:M2) should give a ref-error for both
    await assertReply("setRangeValues", {
      uri, sheetId, cells: {0:{12:{"v":null,"p":null,"f":null,"si":null,"custom":null}}},
    },
      "setCells", {[uri]: {[sheetId]: [
          [[0,13],refError],
      ]}}
    );

    logger.info(`>>> shutting down TestClient`)
    await testClient.stop();
    await new Promise<void>((resolve, reject) => {
      testClient.process!.on('exit', (_code) => resolve());
      testClient.process!.on('error', reject);
    });

    // lift errors from the notification handler (after shutting down client.)
    if (errors.length > 0) {
      const message = errors.map((e, i) => `\n(${i + 1}) ${e.message}`).join("\n");
      const sometimes = errors.length > 1 ? ` (${errors.length}x)` : "";
      throw new Error(`assertReply failed${sometimes}:\n${message}`);
    }
  });
});
