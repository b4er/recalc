import React, { useState } from "react";

import { ColorKit, IColumnRange, IRowRange } from "@univerjs/core";

import { IMarkSelectionService } from "@univerjs/sheets-ui";
import { ISidebarService, useDependency } from "@univerjs/ui";
import { ISelectionWithStyle } from "@univerjs/sheets";

const nanoid = require("nanoid");

import { parseCellRange } from "../../../parser";

export type CellRange = IRowRange & IColumnRange;

export type RangeSelectorProps = {
  subUnitId: string,
  initialValue?: string,
  color: string,
  updateRange: (value?: CellRange) => void,
}

export function RangeSelector({updateRange, initialValue, color}: RangeSelectorProps) {
  const [text, setText] = useState(initialValue || '')
  const [width, setWidth] = useState<number>(initialValue?.length || 0);
  const [parseResult, setParseResult] = useState(parseCellRange(initialValue || ''));
  const [isFocused, setFocused] = useState(false);

  const inputRef = React.useRef<HTMLDivElement>(null);

  const markSelectionService = useDependency(IMarkSelectionService)
  const sidebarService = useDependency(ISidebarService);

  let internalId = React.useMemo(() => nanoid.nanoid(), [])

  const deleteShape = () =>
    [...markSelectionService.getShapeMap().entries()]
      .filter(([_shapeId, v]: [string, {selection: ISelectionWithStyle}]) => v.selection.style?.id === internalId)
      .forEach(([shapeId, _]) =>
        markSelectionService.removeShape(shapeId)
      )

  React.useEffect(() => {
    if (initialValue && inputRef.current) {
      inputRef.current.innerHTML = initialValue;
      restoreCaretPosition(inputRef.current, initialValue.length)
    }
  }, []);

  // maintain cursor and display errors
  React.useEffect(() => {
    if (!(inputRef?.current)) return;

    const target = inputRef.current;
    const text = target.textContent || '';

    // keep the current caret offset
    const caretOffset = getCaretCharacterOffsetWithin(target);

    // annotate the error
    if (!parseResult.status) {
      const ix = parseResult.index.offset;
      const expected = parseResult.expected.join(", ");
      normalizeText(target.innerHTML = (
        text.substring(0, ix) + `<u title="expected ${expected}" style="text-decoration: wavy red underline;">${text.substring(ix)}</u>`
      ));
    } else {
      target.innerHTML = normalizeText(text);
    }

    // restore the caret
    if (text !== "") {
      restoreCaretPosition(target, caretOffset);
    }

    // propagate result
    updateRange(parseResult.status ? parseResult.value : undefined)
  }, [parseResult])

  // visually mark the range selections
  React.useEffect(() => {
    deleteShape();

    if (sidebarService.visible && parseResult.status) {
      const range = parseResult.value;

      markSelectionService.addShape({
        style: {
          // removing by shape-id leaves us with shapes leaking
          id: internalId,
          fill: new ColorKit(color).setAlpha(0.07).toRgbString(),
          stroke: color,
          strokeWidth: isFocused ? 2 : 1,
          strokeDash: isFocused ? 10 : undefined,
          widgets: {},
        },
        range: range,
        primary: null,
      })
    }
  }, [parseResult, sidebarService.visible, isFocused])

  // update width of input div and parse result when text content changes
  React.useEffect(() => {
    setWidth(text.length);
    setParseResult(parseCellRange(text));
  }, [text])

  React.useEffect(() => deleteShape, []);

  return (
    <div
      className="range-selector"
      ref={inputRef}
      contentEditable={true}
      onInput={ev => {
        setText(ev.currentTarget.textContent || '')
      }}
      onFocus={_ => {
        setFocused(true);
      }}
      style={{width: `${width+1}ch`, background: color}}
    />
  );
}

function restoreCaretPosition(element: HTMLDivElement, offset: number) {
  const range = document.createRange();
  const selection = window.getSelection();
  let currentOffset = 0;

  const traverseNodes = (node: ChildNode) => {
    if (node.nodeType === Node.TEXT_NODE) {
      const nodeLength = node.textContent?.length || 0;
      if (currentOffset + nodeLength >= offset) {
        range.setStart(node, offset - currentOffset);
        range.setEnd(node, offset - currentOffset);
        return true; // Stop traversing
      }
      currentOffset += nodeLength;
    } else if (node.nodeType === Node.ELEMENT_NODE) {
      for (const child of node.childNodes) {
        if (traverseNodes(child)) return true;
      }
    }
    return false;
  };

  traverseNodes(element);
  selection?.removeAllRanges();
  selection?.addRange(range);
};

function getCaretCharacterOffsetWithin(element: HTMLDivElement): number {
  const selection = window.getSelection();

  if (!selection?.rangeCount) {
    return 0;
  }

  const range = selection.getRangeAt(0);
  const preCaretRange = range.cloneRange();
  preCaretRange.selectNodeContents(element);
  preCaretRange.setEnd(range.endContainer, range.endOffset);
  return preCaretRange.toString().length;
};

function normalizeText(input: string) {
  return input.replace(/\n/g, "").trim()
}
