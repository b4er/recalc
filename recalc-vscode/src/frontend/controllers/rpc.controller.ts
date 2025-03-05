import { WebviewApi } from "vscode-webview";

import { Disposable, ICommandInfo, ICommandService, IExecutionOptions, IUniverInstanceService, UniverInstanceType, Workbook } from "@univerjs/core";
import { IInsertSheetMutationParams, IRemoveSheetMutationParams, ISetRangeValuesMutationParams, ISetWorksheetNameMutationParams, ISetWorksheetOrderMutationParams, InsertSheetMutation, RemoveSheetMutation, SetRangeValuesMutation, SetWorksheetNameMutation, SetWorksheetOrderMutation } from "@univerjs/sheets";

import { Loglevel } from "../../rpc/logging";

const vscode: WebviewApi<never> = acquireVsCodeApi();

export type Message<M extends keyof SpreadsheetProtocol> = {
  method: M;
  params: Omit<SpreadsheetProtocol[M]["params"], "uri">;
  notification?: boolean,
}

export function postMessage<M extends keyof SpreadsheetProtocol>(message: Message<M>): void {
  return vscode.postMessage(message);
}

export type LogMessage = {
  method: "log";
  params: {
    level: Loglevel;
    message: string;
  };
}

export const logger = {
  error: (message: string) =>
    vscode.postMessage({method: "log", params: {level: Loglevel.Error, message: message}}),
  warn: (message: string) =>
    vscode.postMessage({method: "log", params: {level: Loglevel.Warning, message: message}}),
  info: (message: string) =>
    vscode.postMessage({method: "log", params: {level: Loglevel.Info, message: message}}),
  log: (message: string) =>
    vscode.postMessage({method: "log", params: {level: Loglevel.Debug, message: message}}),
}

export class MessageController extends Disposable {
  constructor(
    @ICommandService private readonly _commandService: ICommandService,
    @IUniverInstanceService private readonly _instanceService: IUniverInstanceService,
  ) {
    super();

    // handle incoming messages
    window.addEventListener('message', message => this.handleMessage(message));

    this.disposeWithMe(this._commandService.onCommandExecuted(
      (command: ICommandInfo, options?: IExecutionOptions) => {

        if (!command.params || (options && options.onlyLocal === true))
          return;

        switch (command.id) {
          case SetRangeValuesMutation.id:
            return this.handleSetRangeValues(command.params as ISetRangeValuesMutationParams)
          case InsertSheetMutation.id:
            return this.handleInsertSheet(command.params as IInsertSheetMutationParams)
          case RemoveSheetMutation.id:
            return this.handleRemoveSheet(command.params as IRemoveSheetMutationParams)
          case SetWorksheetOrderMutation.id:
            return this.handleSetWorksheetOrder(command.params as ISetWorksheetOrderMutationParams)
          case SetWorksheetNameMutation.id:
            return this.handleSetWorksheetName(command.params as ISetWorksheetNameMutationParams)
        }
    }))
  }

  override dispose() {
    window.removeEventListener('message', message => this.handleMessage(message));
    super.dispose()
  }

  /**
   * Handle an RPC message originating from the server-exe.
   * @param event message event received
   */
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  private handleMessage(event: MessageEvent<any>) {
    if (!event?.data?.method || !event?.data?.params) {
      return;
    }

    const workbook = this._instanceService.getCurrentUnitForType<Workbook>(UniverInstanceType.UNIVER_SHEET);

    for (const [sheetName, value] of Object.entries(event.data.params)) {
      const sheetId = workbook?.getSheetBySheetName(sheetName)?.getSheetId();

      if (!sheetId) {
        logger.warn(`invalid call to 'setCells': cannot get sheet ID (${JSON.stringify({sheetName: sheetName, value: value})})`)
        continue;
      }

      this._commandService.executeCommand(SetRangeValuesMutation.id, {
        unitId: data.id,
        subUnitId: sheetId,
        cellValue: value,
        loop: true,
      })
    }
  }

  /* Command handlers (forward to server via JSON-RPC) */

  private handleSetRangeValues(params: ISetRangeValuesMutationParams & {loop?: boolean}) {
    if (params.loop) return;

    const workbook = this._instanceService.getCurrentUnitForType<Workbook>(UniverInstanceType.UNIVER_SHEET);
    const sheetName = workbook?.getSheetBySheetId(params.subUnitId)?.getName();

    if (!sheetName) {
      logger.error(`invalid call to 'setRangeValues': cannot get sheet name (${JSON.stringify(params)})`)
      return;
    }

    postMessage({method: "setRangeValues", params: {
      sheetId: sheetName,
      cells: params.cellValue,
    }});
  }

  private handleInsertSheet(params: IInsertSheetMutationParams) {
    postMessage({method: "insertSheet", params: {
      index: params.index,
      sheetId: params.sheet.id,
      sheetName: params.sheet.name,
    }});
  }

  private handleRemoveSheet(params: IRemoveSheetMutationParams) {
    postMessage({method: "removeSheet", params: {
      sheetName: params.subUnitId,
    }});
  }

  private handleSetWorksheetOrder(params: ISetWorksheetOrderMutationParams) {
    postMessage({method: "setWorksheetOrder", params: {
      sheetId: params.subUnitId,
      from: params.fromOrder,
      to: params.toOrder,
    }});
  }

  private handleSetWorksheetName(params: ISetWorksheetNameMutationParams) {
    postMessage({method: "setWorksheetName", params: {
      sheetName: params.subUnitId,
      newName: params.name,
    }});
  }
}
