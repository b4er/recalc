import { IScale, IRange, Range, IDisposable } from "@univerjs/core";
import { IDrawInfo, SheetExtension, SpreadsheetExtensionRegistry, SpreadsheetSkeleton, UniverRenderingContext } from "@univerjs/engine-render";

import './cell-diagnostics.css'

export class CellFormatting extends SheetExtension {
  public static addExtension(): IDisposable {
    SpreadsheetExtensionRegistry.add(CellFormatting);
    return { dispose: () => SpreadsheetExtensionRegistry.delete(CellFormatting) };
  }

  override draw(ctx: UniverRenderingContext, _parentScale: IScale, skeleton: SpreadsheetSkeleton, _diffBounds?: IRange[] | undefined, _more?: IDrawInfo | undefined): void {
    const {worksheet} = skeleton;

    if (!worksheet) {
      return;
    }

    Range.foreach(skeleton.rowColumnSegment, (row, col) => {
      if (!worksheet.getRowVisible(row) || !worksheet.getColVisible(col)) {
        return;
      }

      const cellData = worksheet.getCell(row, col);
      if (cellData?.custom !== undefined && (cellData?.custom?.errors || cellData?.custom?.warnings)) {
        let { isMerged, isMergedMainCell, mergeInfo, startY, endY, startX, endX } = skeleton.getCellWithCoordByIndex(row, col);
        if (isMerged) {
            return;
        }
        if (isMergedMainCell) {
            startY = mergeInfo.startY;
            endY = mergeInfo.endY;
            startX = mergeInfo.startX;
            endX = mergeInfo.endX;
        }

        const PADDING = 1;

        // magic.
        const x0 = startX-45;
        const y0 = startY-19;

        const color = cellData?.custom?.errors ? 'rgba(235, 51, 58, 0.32)' : 'rgba(255, 175, 0, 0.44)';
        ctx.fillStyle = this.stripePattern(ctx, color);
        ctx.lineWidth = 2;

        this.drawRectWithRoundedCorner(ctx, x0, y0, endX-startX-PADDING, endY-startY-PADDING);
      }
    })
  }

  private stripePattern(ctx: UniverRenderingContext, color: string): CanvasPattern | string {
    const pattern = new OffscreenCanvas(32,16);
    const pctx = pattern.getContext("2d");

    if (pctx) {
      const x0=36;
      const x1=-4;
      const y0=-2;
      const y1=18;
      const offset=32;

      pctx.strokeStyle = color;
      pctx.lineWidth=4;
      pctx.beginPath();
      pctx.moveTo(x0,y0);
      pctx.lineTo(x1,y1);
      pctx.moveTo(x0-offset,y0);
      pctx.lineTo(x1-offset,y1);
      pctx.moveTo(x0+offset,y0);
      pctx.lineTo(x1+offset,y1);
      pctx.stroke();

      const p = ctx.createPattern(pattern, 'repeat');
      return !p ? color : p;
    }

    return color;
  }

  private drawRectWithRoundedCorner(ctx: UniverRenderingContext, x: number, y: number, width: number, height: number) {
    const radius = 5;
    if (!height || !width) {
        return;
    }
    ctx.beginPath();

    ctx.moveTo(x + radius, y);
    ctx.lineTo(x + width - radius, y);
    ctx.arcTo(x + width, y, x + width, y + radius, radius);
    ctx.lineTo(x + width, y + height - radius);
    ctx.arcTo(x + width, y + height, x + width - radius, y + height, radius);
    ctx.lineTo(x + radius, y + height);
    ctx.arcTo(x, y + height, x, y + height - radius, radius);
    ctx.lineTo(x, y + radius);
    ctx.arcTo(x, y, x + radius, y, radius);

    ctx.closePath();
    ctx.fill();
  }
}
