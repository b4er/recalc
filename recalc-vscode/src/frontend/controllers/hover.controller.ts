import React from 'react';

import type { ICellDataForSheetInterceptor, Nullable, Workbook } from '@univerjs/core';
import { Disposable, Inject, IUniverInstanceService, UniverInstanceType } from '@univerjs/core';
import { IRenderManagerService } from '@univerjs/engine-render';
import { CellAlertManagerService, CellAlertType, HoverManagerService, SheetSkeletonManagerService } from '@univerjs/sheets-ui';
import { debounceTime } from 'rxjs';

import { CellFormatting } from '../views/extensions/cell-diagnostics.render';

type Annotation = { title: string, message: string};

export class HoverController extends Disposable {
  static KEY = "hover-controller-key";

  constructor(
      @Inject(HoverManagerService) private readonly _hoverManagerService: HoverManagerService,
      @Inject(CellAlertManagerService) private readonly _cellAlertManagerService: CellAlertManagerService,
      @IUniverInstanceService private readonly _univerInstanceService: IUniverInstanceService,
      @IRenderManagerService private readonly _renderManagerService: IRenderManagerService,
  ) {
    super();
    this.disposeWithMe(CellFormatting.addExtension());
    this.disposeWithMe(this._hoverManagerService.currentCell$.pipe(debounceTime(100)).subscribe((cellPos) => {
      if (cellPos) {
        const workbook = this._univerInstanceService.getCurrentUnitForType<Workbook>(UniverInstanceType.UNIVER_SHEET)!;
        const worksheet = workbook.getActiveSheet();
        if (!worksheet) return;

        const cellData: Nullable<ICellDataForSheetInterceptor> = worksheet.getCell(cellPos.location.row, cellPos.location.col);

        // FIXME: deal with hovering over merged cells
        // const mergedCell = skeleton.worksheet.getMergedCell(cellPos.location.row, cellPos.location.col);

        if (cellData?.custom && (cellData?.custom.errors || cellData?.custom.warnings)) {
          const skeleton = this._renderManagerService.getRenderById(cellPos.location.unitId)
            ?.with(SheetSkeletonManagerService)
            ?.getSkeleton(cellPos.location.subUnitId);

          if (skeleton != undefined) {
            let i = 0;
            (cellData?.custom.errors || []).forEach((err: Annotation) =>
              _cellAlertManagerService.showAlert({
                type: CellAlertType.ERROR,
                title: err.title,
                message: MessageElement(err.message),
                location: cellPos.location,
                width: 320,
                height: 74 + (i++)*75,
                key: HoverController.KEY
              })
            );

            (cellData?.custom.warnings || []).forEach((err: Annotation) =>
              _cellAlertManagerService.showAlert({
                type: CellAlertType.WARNING,
                title: err.title,
                message: MessageElement(err.message),
                location: cellPos.location,
                width: 320,
                height: 74 + (i++)*75,
                key: HoverController.KEY
              })
            );
          }
        } else {
          _cellAlertManagerService.removeAlert(HoverController.KEY)
        }
      }
    }));
  }
}

/**
 * Split a raw string on its newlines and produce HTML from it.
 *
 * @param errorMessage newline-separated string containing an error message (example: a rendered Megaparsec error message)
 * @returns React element with proper line breaks (as <br>)
 */
function MessageElement(errorMessage: string) {
  return React.createElement(
    React.Fragment,
    {},
    ...(errorMessage.split("\n").map(line => [
      line,
      React.createElement("br"),
    ])).flat()
  )
}
