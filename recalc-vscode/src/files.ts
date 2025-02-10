import { IWorksheetData } from "@univerjs/core";
import { readFileSync } from "node:fs";
/**
 * Utilities for reading SheetDocuments (file-ending: .rc).
 *
 * The spreadsheet is represented as a JSON object with a `sheetOrder` array and a `sheets`
 * object, where each sheet is a map of row and column indices to `CELL` data.
 *
 * A `CELL` contains fields like:
 * - `v?` (value): The cell's value.
 * - `f?` (formula): A raw formula string (e.g., `=SUM(A1:B4)`).
 *
 * Other fields like `s` (style information) are also supported, see Univer documentation.
 *
 * Functions:
 * - `readJson(uri: IUri, text: string): SheetDocument`: Reads a JSON string and returns a `SheetDocument`.
 * - `openFile(uri: IUri): SheetDocument`: Opens a file from the URI and returns a `SheetDocument`.
 *
 */

const nanoid = require("nanoid")

type IUri = {
  path: string,
  toString: () => string,
}

export type SheetDocument = {
	id: string,
	sheetOrder: [string, string][],
	sheets: {
		[sheetId: string]: Partial<IWorksheetData>
	}
};

export function readJson(uri: IUri, text: string): SheetDocument {
  const json = JSON.parse(text);

  // validate fields a bit
  if (json.sheetOrder !== undefined && json.sheets !== undefined) {
    if (Array.isArray(json.sheetOrder)) {
      if (typeof json.sheets === 'object') {
        // generate an ID per sheet-name
        const randomIds = new Map(
          Object.keys(json.sheets).map(sheetName => [sheetName, nanoid.nanoid()])
        );

        return {
          id: btoa(uri.toString()),
          // map random ids to names
          sheetOrder: json.sheetOrder.map((x: string) => [randomIds.get(x)!, x]),
          // check & build .sheets entries
          sheets: Object.fromEntries(
            json.sheetOrder.map(
              (sheetName: string) => [randomIds.get(sheetName), {
								id: randomIds.get(sheetName),
                name: sheetName,
                ...validateSheet(sheetName, json.sheets[sheetName])
              }]
            )
          )
        }
      }
      throw new Error(".sheets must be an object {[sheetName]: ...}")
    }

    throw new Error("sheetOrder not an array")
  }

  throw new Error(".sheetOrder and .sheets not an array must be defined")
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function validateSheet(sheetName: string, sheetJson: any) {
  if (sheetJson === undefined) {
    throw new Error(`.sheets.${sheetName} cannot be undefined`);
  }

  if (sheetJson.name !== undefined) {
    throw Error(`.sheets.${sheetName} must not override .name`)
  }

  return sheetJson;
}

export function openFile(uri: IUri): SheetDocument {
  return readJson(uri, readFileSync(uri.path).toString('utf-8'));
}
