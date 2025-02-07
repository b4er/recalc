import { LocaleType, merge } from '@univerjs/core';

import DesignEnUS from '@univerjs/design/locale/en-US';
import UIEnUS from '@univerjs/ui/locale/en-US';
import DocsUIEnUS from '@univerjs/docs-ui/locale/en-US';
import SheetsEnUS from '@univerjs/sheets/locale/en-US';
import SheetsUIEnUS from '@univerjs/sheets-ui/locale/en-US';
import SheetsFormulaUIEnUS from '@univerjs/sheets-formula-ui/locale/en-US';

const enUS = merge(
  SheetsEnUS,
  DocsUIEnUS,
  SheetsUIEnUS,
  SheetsFormulaUIEnUS,
  UIEnUS,
  DesignEnUS,
);

import DesignZhCN from '@univerjs/design/locale/zh-CN';
import UIZhCN from '@univerjs/ui/locale/zh-CN';
import DocsUIZhCN from '@univerjs/docs-ui/locale/zh-CN';
import SheetsZhCN from '@univerjs/sheets/locale/zh-CN';
import SheetsUIZhCN from '@univerjs/sheets-ui/locale/zh-CN';
import SheetsFormulaUIZhCN from '@univerjs/sheets-formula-ui/locale/zh-CN';

const zhCN = merge(
  SheetsZhCN,
  DocsUIZhCN,
  SheetsUIZhCN,
  SheetsFormulaUIZhCN,
  UIZhCN,
  DesignZhCN,
);

export const locales = {
    [LocaleType.EN_US]: enUS,
    [LocaleType.ZH_CN]: zhCN,
}
