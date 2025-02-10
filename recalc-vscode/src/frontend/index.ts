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
import { AddRangeProtectionFromToolbarCommand, UniverSheetsUIPlugin } from "@univerjs/sheets-ui";

import { UniverFormulaEnginePlugin } from "@univerjs/engine-formula";

import { locales } from './locales';
import { RecalcPlugin } from "./plugin";
import { UniverSheetsFormulaUIPlugin } from "@univerjs/sheets-formula-ui";

// basic example (no interaction with the backend)
const univer = new Univer({
  theme: defaultTheme,
  locale: LocaleType[config.locale],
  locales: locales,
  logLevel: LogLevel.VERBOSE,
});

univer.registerPlugin(UniverRenderEnginePlugin);
univer.registerPlugin(UniverFormulaEnginePlugin);

univer.registerPlugin(UniverUIPlugin, {
  container: 'app',
  menu: {
    ['sheet.menu.sheet-frozen']: { hidden: true },
    ['sheet.contextMenu.permission']: { hidden: true },
    [ AddRangeProtectionFromToolbarCommand.id ]: { hidden: true },
  }
});

/* register all necessary plugins */
univer.registerPlugin(UniverDocsPlugin);
univer.registerPlugin(UniverDocsUIPlugin);

univer.registerPlugin(UniverSheetsPlugin);
univer.registerPlugin(UniverSheetsUIPlugin);
univer.registerPlugin(UniverSheetsFormulaUIPlugin);

/* register the recalc-plugin */
univer.registerPlugin(RecalcPlugin);

// create univer sheet using the fresh subUnitIds
univer.createUnit(UniverInstanceType.UNIVER_SHEET, {
  ...data,
  sheetOrder: data.sheetOrder.map((x: [string, string]) => x[0])
});
