import React from "react";
import { ParseResult } from "../../../parser";

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

export type ParseInputProps<T> = {
  initialValue?: string,
  className: string,
  parse: (input: string) => ParseResult<T>,
  setValue: (newValue: T) => void
}

export function ParseInput<T>({initialValue, className, parse, setValue}: ParseInputProps<T>) {
  const inputRef = React.useRef<HTMLDivElement>(null);
  const lastValidValue = React.useRef<string>(initialValue || '');

  const handleInput = () => {
    const inputDiv = inputRef.current;
    if (!inputDiv) return;

    const caretPosition = getCaretCharacterOffsetWithin(inputDiv);

    const userInput = inputDiv.textContent || ''
    const result = parse(userInput);

    if (result.status === true) {
      setValue(result.value);
      lastValidValue.current = userInput;
      inputDiv.innerHTML = userInput;
      restoreCaretPosition(inputDiv, caretPosition);
    } else {
      const errorIndex = result.index.offset;
      const expected = result.expected.join(", ");
      const token = userInput.substring(result.index.offset).split(/\s+/)[0];
      const errorMessage = `unexpected '${token}', expected ${expected}`;

      inputDiv.innerHTML = (
        userInput.substring(0, errorIndex) +
        `<u title="${errorMessage}" style="text-decoration: wavy red underline;">` +
        `${userInput.substring(errorIndex)}` +
        `</u>`
      );

      restoreCaretPosition(inputDiv, caretPosition);
    }
  }

  React.useEffect(() => {
    if (inputRef.current) {
      inputRef.current.textContent = initialValue || '';
    }
  }, [initialValue]);

  return (
    <div
      ref={inputRef}
      className={className}
      contentEditable
      onInput={handleInput}
      style={{ wordWrap: "break-word", outline: "none", whiteSpace: "pre-wrap" }}
    />
  );
}
