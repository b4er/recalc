import { useState } from "react";
import { IColumnRange, IRowRange, LocaleService, useDependency } from "@univerjs/core";

import './SheetFunctionContainer.css'

export type CellRange = IRowRange & IColumnRange;

type FunctionDefinition = {description: string};

export function SheetFunctionContainer() {
  const localeService = useDependency(LocaleService);

  const [state, setState] = useState<FunctionDefinition>({
    description: ""
  });

  return (
    <div className="range-container">
      <div className="description-section">
        <h3>{localeService.t('Description')}</h3>

        <textarea
            className="description-textfield"
            value={state.description}
            onChange={(e) => setState({ ...state, description: e.target.value })}
            placeholder={localeService.t('Enter description')}
          />
      </div>

      <div className="sheet-function-inputs-container">
        <h3>{localeService.t('Inputs')}</h3>
      </div>

      <div className="outputs-section sheet-function-inputs-container">
        <h3>{localeService.t('Outputs')}</h3>
      </div>
    </div>
  );
}
