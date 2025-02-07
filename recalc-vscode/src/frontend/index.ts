import "@univerjs/design/lib/index.css";
import "@univerjs/ui/lib/index.css";
import "@univerjs/docs-ui/lib/index.css";
import "@univerjs/sheets-ui/lib/index.css";
import "@univerjs/sheets-formula-ui/lib/index.css";

import { Univer, LocaleType, LogLevel, UniverInstanceType } from '@univerjs/core';
import { defaultTheme } from "@univerjs/design";

import { UniverRenderEnginePlugin } from "@univerjs/engine-render";

import { UniverUIPlugin } from "@univerjs/ui";

import { UniverDocsPlugin } from "@univerjs/docs";
import { UniverDocsUIPlugin } from "@univerjs/docs-ui";

import { UniverSheetsPlugin } from "@univerjs/sheets";
import { UniverSheetsUIPlugin } from "@univerjs/sheets-ui";

import { UniverFormulaEnginePlugin } from "@univerjs/engine-formula";
import { UniverSheetsFormulaPlugin } from "@univerjs/sheets-formula";

import { locales } from './locales';

// basic example (no interaction with the backend)
// (refer to: https://docs.univer.ai/en-US/guides/sheets/features/core/sheet-api)
const univer = new Univer({
  theme: defaultTheme,
  locale: LocaleType[config.locale],
  locales: locales,
  logLevel: LogLevel.VERBOSE,
});

univer.registerPlugin(UniverRenderEnginePlugin);
univer.registerPlugin(UniverFormulaEnginePlugin);

univer.registerPlugin(UniverUIPlugin, {
  container: 'app'
});

univer.registerPlugin(UniverDocsPlugin);
univer.registerPlugin(UniverDocsUIPlugin);

univer.registerPlugin(UniverSheetsPlugin);
univer.registerPlugin(UniverSheetsUIPlugin);

/* register the Univer formula engine */
univer.registerPlugin(UniverSheetsFormulaPlugin);

// create univer sheet using the fresh subUnitIds
univer.createUnit(UniverInstanceType.UNIVER_SHEET, {
  ...data,
  sheetOrder: data.sheetOrder.map((x: [string, string]) => x[0])
});
