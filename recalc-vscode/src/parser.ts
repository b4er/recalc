// Import a parser combinator library such as "parsimmon"
import { IColumnRange, IRowRange } from '@univerjs/core';
import * as P from 'parsimmon';

function excel26(col: string): number {
  let result = 0;
  for (let i = 0; i < col.length; i++) {
    result = result * 26 + (col.charCodeAt(i) - "A".charCodeAt(0) + 1);
  }
  return result;
}

function showExcel(col: number): string {
  let result = '';
  while (col >= 0) {
    result = String.fromCharCode((col % 26) + 65) + result;
    col = Math.floor(col / 26) - 1;
  }
  return result;
}

function lexeme<T>(p: P.Parser<T>): P.Parser<T> {
  return p.skip(P.optWhitespace)
}

const RowIndex = P.regexp(/[1-9][0-9]*/).map((x: string) => Number(x)-1)
  .desc("row index");
const ColIndex = P.regexp(/[A-Z]+/).map((letters: string) => excel26(letters)-1)
  .desc("column index");

// @ts-expect-error: errors
const CellIndex = lexeme(P.seqObj(["column", ColIndex], ["row", RowIndex]))
const CellRange = P.seqObj(
  P.optWhitespace, // @ts-expect-error: errors
  ["start", CellIndex],
  ["end", lexeme(P.string(":")).then(CellIndex).fallback(null)], // @ts-expect-error: errors
).map((x: {start: CellAddr, end?: CellAddr}) => ({
  startRow: x.start.row,
  startColumn: x.start.column,
  endRow: x.end ? x.end.row : x.start.row,
  endColumn: x.end ? x.end.column : x.start.column,
}));

type CellAddr = {row: number, column: number};

export type CellRange = IRowRange & IColumnRange;

export type ParseResult<T>
  = {status: true, value: T}
  | {status: false, index: {offset: number, line: number, column: number}, expected: string[]}

export function parseCellRange(input: string): ParseResult<CellRange> {
  return CellRange.parse(input);
}

function showCellAddr(row: number, column: number): string {
  return `${showExcel(column)}${row+1}`
}

export function showCellRange({startRow, startColumn, endRow, endColumn}: CellRange): string {
  const startString = showCellAddr(startRow, startColumn);

  if (startRow === endRow && startColumn == endColumn) {
    return startString;
  }
  return `${startString}:${showCellAddr(endRow, endColumn)}`
}

export function parseVariableName(input: string): ParseResult<string> {
  let l = 0, c = 0;
  for (let i = 0; i < input.length; i++) {
    const char = input.charAt(i);
    if (!/^[a-zA-Z0-9 \._]$/.test(char)) {
      return {status: false, index: {offset: i, line: l, column: c}, expected: ["variable name"]}
    }
    if (char === "\n") {
      c = 0;
      l += 1;
    } else {
      c += 1;
    }
  }

  return {status: true, value: input.trim()}
}
