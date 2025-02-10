import * as fs from "node:fs";

const filePath = process.env.LOG_FILE || "recalc-vscode.test.log";

const timestamp = (): string => new Date().toISOString();

export const fileLogger = (filePath: string) => {
  if (fs.existsSync(filePath)) {
    fs.appendFileSync(filePath, `--------------------------------\n`)
    fs.appendFileSync(filePath, `--- ${timestamp()} ---\n`)
    fs.appendFileSync(filePath, `--------------------------------\n`)
  }

  function output(level: string) {
    return (message: string) => fs.appendFileSync(
      filePath, `${timestamp()} [${level}] ${message}\n`
    )
  }

  return {
    error: output("ERROR"),
    warn: output("WARN"),
    info: output("INFO"),
    log: output("DEBUG"),
  }
};

const noop = (_: string) => {};

export const noLogger = {error: noop, warn: noop, info: noop, log: noop};

export const logger = process.env.LOG_OUTPUT === "true" ? fileLogger(filePath) : noLogger;
