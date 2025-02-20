import assert from "node:assert";
import 'mocha';
import * as fs from "node:fs";
import * as rpc from 'vscode-jsonrpc/node';

import { Client, MessageTransports } from "../../rpc/client";
import { ChildProcessWithoutNullStreams, execSync, spawn } from "node:child_process";
import { logger } from "../logging";

const nanoid = require("nanoid")

const NAME = "test";

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
    this.process.stderr.on('data', data => this.logger.info(`(${binName}) ${data.toString().trim()}`));

    return Promise.resolve({reader: reader, writer: writer})
  }
};

describe('testClient', function () {
  this.timeout(5000);

  it('should successfully send a "open" request and receive a response', async () => {
    const testClient = new TestClient();

    logger.info(`>>> starting TestClient`)
    await testClient.start();

    logger.info(`>>> send open request to TestClient`)
    const result = await testClient.request("open", {uri: "test://file.rc", sheetOrder: [[nanoid.nanoid(), "Sheet 1"]]})

    assert.deepEqual(result, []);

    logger.info(`>>> shutting down TestClient`)
    await testClient.stop();
    await new Promise<void>((resolve, reject) => {
      testClient.process!.on('exit', (_code) => resolve());
      testClient.process!.on('error', reject);
    });
  });
});
