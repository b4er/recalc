import { WebviewApi } from "vscode-webview";

import { Disposable, ICommandInfo, ICommandService, IExecutionOptions, Inject, IUniverInstanceService, UniverInstanceType, Workbook } from "@univerjs/core";
import { IInsertSheetMutationParams, IRemoveSheetMutationParams, ISetRangeValuesMutationParams, ISetWorksheetNameMutationParams, ISetWorksheetOrderMutationParams, InsertSheetMutation, RemoveSheetMutation, SetRangeValuesMutation, SetWorksheetNameMutation, SetWorksheetOrderMutation } from "@univerjs/sheets";

import { Loglevel } from "../../rpc/logging";
import { IDescriptionService } from "@univerjs/sheets-formula";
import { DescriptionService } from "../services/function-description.service";

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
    @Inject(IDescriptionService) private readonly _descriptionService: IDescriptionService,
    @IUniverInstanceService private readonly _instanceService: IUniverInstanceService,
  ) {
    super();

    // handle incoming messages
    window.addEventListener('message', message => this.handleMessage(message));

    // handle before command's action has taken place (i.e. values are still the old ones)
    this.disposeWithMe(this._commandService.beforeCommandExecuted((command: ICommandInfo, options?: IExecutionOptions) => {
      if (!command.params || (options && options.onlyLocal === true))
        return;

      switch (command.id) {
        case SetWorksheetNameMutation.id:
          return this.handleSetWorksheetName(command.params as ISetWorksheetNameMutationParams)
      }
    }));

    // usually we want to handle after the fact to make sure our actions take precedence
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
        }
    }));
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

    switch (event.data.method) {
      case "defineFunction":
        // alert the error if one occurred
        if ("Left" in event.data.params) {
          logger.error(event.data.params.Left)
        // otherwise, add the register the defined function with the DescriptionService
        } else if ("Right" in event.data.params) {
          const defs: FunctionDescription[] = event.data.params.Right;
          this._descriptionService.registerDescriptions(
            defs.map(def => DescriptionService.functionDescription2Info(def))
          );
          const names = defs.map(def => `'${def.name}'`).join(', ');
          logger.info(`Successfully stored function${defs.length > 1 ? 's ' : ' '}${names}.`);
        } else {
          logger.error(`invalid 'defineFunction' (${JSON.stringify(event.data.params)})`)
        }
        break;
      case "setRangeValues":

        const workbook = this._instanceService.getCurrentUnitForType<Workbook>(UniverInstanceType.UNIVER_SHEET);

        for (const [sheetName, value] of Object.entries(event.data.params)) {
          const sheetId = workbook?.getSheetBySheetName(sheetName)?.getSheetId();

          if (!sheetId) {
            logger.warn(`invalid call to 'setRangeValues': cannot get sheet ID (${JSON.stringify({sheetName: sheetName, value: value})})`)
            continue;
          }

          this._commandService.executeCommand(SetRangeValuesMutation.id, {
            unitId: data.id,
            subUnitId: sheetId,
            cellValue: value,
            loop: true,
          })
        }
        break;
      default:
        break;
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
      sheetName: sheetName,
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
      sheetName: this.nameById(params.subUnitId),
      from: params.fromOrder,
      to: params.toOrder,
    }});
  }

  private handleSetWorksheetName(params: ISetWorksheetNameMutationParams) {
    // the old name is given by the instanceService since it has not been changed yet
    const oldName = this.nameById(params.subUnitId);

    // in case the sheet describes a function, rename it:
    const oldFunctionInfo = this._descriptionService.getFunctionInfo(oldName);
    if (oldFunctionInfo) {
      this._descriptionService.registerDescriptions([{...oldFunctionInfo, functionName: params.name}]);
    }
    this._descriptionService.unregisterDescriptions([oldName]);

    // let the language server know about the name change
    postMessage({method: "setWorksheetName", params: {
      sheetName: oldName,
      newName: params.name,
    }});
  }

  private nameById(subUnitId: string) {
    const workbook = this._instanceService.getCurrentUnitForType<Workbook>(UniverInstanceType.UNIVER_SHEET);
    const sheetName = workbook?.getSheetBySheetId(subUnitId)?.getName();
    if (!sheetName) {
      throw Error(`invalid subUnitId: ${subUnitId}, cannot associate it with a`);
    }

    return sheetName;
  }
}
