import React, { useState } from "react";
import { DragDropContext, Draggable, DraggableProvided, Droppable, DroppableProvided, OnDragEndResponder } from "@hello-pangea/dnd";
import { IColumnRange, IRowRange, LocaleService } from "@univerjs/core";
import { useDependency } from "@univerjs/ui";

const nanoid = require("nanoid");

import { logger, postMessage } from "../../controllers/rpc.controller";
import { ParseInput, ParseInputProps } from "../components/ParseInput";
import { RangeSelector } from "../components/RangeSelector";
import { outputColor, useColor } from "./hooks/useColor";
import { useSidebarClose } from "./hooks/useSidebar";
import { parseVariableName } from "../../../parser";

import './SheetFunctionContainer.css'
import { useActiveWorkbook } from "@univerjs/sheets-ui";
import { useActiveSheet } from "./hooks/useActiveWorksheet";

export type CellRange = IRowRange & IColumnRange;

type Sheet = { unitId?: string, subUnitId?: string, name?: string };

/* Sheet Function Inputs */

type Input = {id: string, name: string, color: string, range?: CellRange};

const InputName = (props: Omit<ParseInputProps<string>, "className" | "parse">) =>
  ParseInput({...props, className: "range-label", parse: parseVariableName});

type SheetFunctionInputsProps = {
  sheet: Sheet,
  deleteInput: (index: number) => void,
  inputs: Input[],
  updateAt: <K extends keyof Input>(index: number, key: K, value: Input[K]) => void
}

const SheetFunctionInputs = React.memo(({ sheet: {subUnitId}, inputs, deleteInput, updateAt }: SheetFunctionInputsProps) => {
  return inputs.map((input, index) => (
    <Draggable draggableId={input.id} index={index} key={input.id}>
      {(provided: DraggableProvided) => {

        const [removeButton, setRemoveButton] = useState<boolean>(false);

        const handleInputContextMenu = (e: React.MouseEvent) => {
          e.preventDefault();
          setRemoveButton(true);
        }

        const onRemoveClicked = () => {
          setRemoveButton(false);
          deleteInput(index);
        };

        return (
          <div
            ref={provided.innerRef}
            {...provided.draggableProps}
            {...provided.dragHandleProps}
            className="range-input"
            style={{...provided.draggableProps.style,
              backgroundColor: input.color,
            }}
            onContextMenu={handleInputContextMenu}
            onClick={_ => setRemoveButton(false)}
          >
            <InputName
              initialValue={input.name}
              setValue={x => updateAt(index, "name", x)} />

            <RangeSelector key={input.id}
              subUnitId={subUnitId!}
              initialValue={`A${index+1}`}
              color={input.color}
              updateRange={range => updateAt(index, "range", range)} />

            <div className="range-input-field"></div>

            <div className={`remove-button ${removeButton ? "visible" : ""}`}
              onClick={onRemoveClicked}
            >
              Remove
            </div>
          </div>
      )}}
    </Draggable>
  ));
});

/* Sheet Function Output */

type SheetFunctionOutputProps = {
  sheet: Sheet,
  setOutputRange: (range?: CellRange) => void,
};

const SheetFunctionOutput = ({sheet: {subUnitId}, setOutputRange}: SheetFunctionOutputProps) =>
  <div className="range-output" style={{backgroundColor: outputColor}}>
    <RangeSelector
      subUnitId={subUnitId!}
      initialValue={`B1`}
      color={outputColor}
      updateRange={setOutputRange} />
  </div>;

/* Sheet Function Editor */

// sheet function editor state
type FunctionDefinition = {
  description: string,
  inputs: Input[],
  output: {range?: CellRange}
};

type SheetFunctionProps = {
  sheet: Sheet,
  state: FunctionDefinition,
  setState: React.Dispatch<React.SetStateAction<FunctionDefinition>>
}

const SheetFunction: React.FC<SheetFunctionProps> = React.memo(({sheet, state, setState}) => {
  // get services
  const localeService = useDependency(LocaleService);

  // hooks and event handling
  const nextColor = useColor();

  // setters
  const updateAt = <K extends keyof Input>(index: number, key: K, value: Input[K]) =>
    setState(st => ({...st, inputs: st.inputs.map((x, i) => ({...x, [key]: i == index ? value : x[key]}))}));

  const addInput = () =>
    setState(st =>
      ({...st, inputs: [...st.inputs, {
        id: nanoid.nanoid(),
        name: `input${st.inputs.length}`,
        color: nextColor(),
      }]})
    )

  const deleteInput = (index: number) =>
    setState(st => ({...st, inputs: st.inputs.filter((_, ix) => ix !== index)}));

  const setOutputRange = (range?: CellRange) => setState(st => ({...st, output: {range}}));

  // handlers
  const onDragEnd: OnDragEndResponder<string> = result => {
    if (!result.destination || result.destination.index === result.source.index) {
      return;
    }
    const destinationIndex = result.destination.index;
    setState(st => ({...st, inputs: reorder(st.inputs, result.source.index, destinationIndex)}));
  }

  /* the component consists of three containers [[Description] [Inputs [input] +] [Output [output]]]

  .range-container
    .description-section {h3; textarea}
    .sheet-function-section
      h3{Inputs}
      (
        .range-input
          .range-label[content-editable]{INPUT NAME}
          .range-selector[content-editable]{CELL-REF}
          .range-input-field{}
          .remove-button{Remove}
      )*
      button{+}
    .sheet-function-section
      h3{Output}
      .range-output
        .range-selector[content-editable]{CELL-REF}

  */
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

      <div className="sheet-function-section">
        <h3>{localeService.t('Inputs')}</h3>

        <DragDropContext onDragEnd={onDragEnd}>
          <Droppable droppableId="list">
            {(provided: DroppableProvided) => (
              <div ref={provided.innerRef} {...provided.droppableProps}>
                <SheetFunctionInputs sheet={sheet} deleteInput={deleteInput} updateAt={updateAt} inputs={state.inputs} />
                {provided.placeholder}
              </div>
            )}
          </Droppable>
        </DragDropContext>

        <button className="add-button" onClick={addInput}>
          + Add Input
        </button>
      </div>

      <div className="sheet-function-section">
        <h3>{localeService.t('Outputs')}</h3>

        <SheetFunctionOutput sheet={sheet} setOutputRange={setOutputRange} />
      </div>
    </div>
  );
});

export function SheetFunctionContainer() {
  // hook the current sheet.{unitId, subUnitId, name}
  const workbook = useActiveWorkbook();
  const worksheet = useActiveSheet();
  const [sheet, setSheet] = useState<Sheet>({});

  React.useEffect(() => {
    setSheet({
      unitId: worksheet?.getUnitId(),
      subUnitId: worksheet?.getSheetId(),
      name: worksheet?.getName(),
    });
  }, [worksheet]);

  // map for each sheet a different FunctionDefinition
  const [state, setState] = useState<{[sheet: string]: FunctionDefinition}>({});

  function getter(subUnitId: string) {
    if (!state[subUnitId]) {
      setState(st => ({...st, [subUnitId]: {
        description: "",
        inputs: [],
        output: {},
      }}))
    }

    return state[subUnitId];
  }

  function setter(subUnitId: string): React.Dispatch<React.SetStateAction<FunctionDefinition>> {
    return (update: FunctionDefinition | ((def: FunctionDefinition) => FunctionDefinition)) => {
      const def = typeof update === 'function' ? update(state[subUnitId]) : update;
      setState(st => ({...st, [subUnitId]: def}));
    }
  }

  useSidebarClose(() => {
    Object.entries(state).forEach(([subUnitId, def]) => {
      const functionName = workbook?.getSheetBySheetId(subUnitId)?.getName();
      if (functionName) {
        saveFunctionDefinition(functionName, def)
      }
    });
  });

  return (
    <div>
      {/* Pass the relevant part of the state to SheetFunction */}
      <SheetFunction
        sheet={ sheet }
        state={ getter(sheet.subUnitId!) }
        setState={setter(sheet.subUnitId!)}
      />
    </div>
  );
}

/* Utility Functions */

function saveFunctionDefinition(functionName: string, state: FunctionDefinition) {
  if (!state.output.range) {
    logger.error(`Cannot define function ‘${functionName}’ with no output!`)
    return;
  }

  postMessage({method: "defineFunction", params: {
    sheetName: functionName,
    description: state.description,
    inputs: state.inputs.flatMap(x => x.range ? [[x.name, tuplifyCellRange(x.range)]] as [[string, CellRangeHs]] : []),
    output: tuplifyCellRange(state.output.range),
  }})
}

type CellRangeHs = [[number,number],[number,number]];

/**
 *
 * @param range Univer cell range
 * @returns serialized as ((Int,Int),(Int,Int))
 */
function tuplifyCellRange(range: CellRange): CellRangeHs {
  return [
    [range.startRow, range.startColumn],
    [range.endRow, range.endColumn],
  ]
}

/**
 * Move element from index to another keeping the array otherwise
 * in the same order (i.e. this does not swap the elements!)
 *
 * Example: [1,2,3] 0 2 -> [2,3,1]
 *
 * @param list array of elements to reorder
 * @param startIndex move element at this position
 * @param endIndex to this position
 * @returns new reordered array
 */
function reorder<T>(list: T[], startIndex: number, endIndex: number): T[] {
  const result = Array.from(list);
  const [removed] = result.splice(startIndex, 1);
  result.splice(endIndex, 0, removed);
  return result;
};
