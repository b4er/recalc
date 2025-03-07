import { IDisposable, toDisposable } from "@univerjs/core";
import { IFunctionNames, IFunctionInfo, IFunctionParam } from "@univerjs/engine-formula";
import { IDescriptionService, ISearchItem } from "@univerjs/sheets-formula";

export class DescriptionService implements IDescriptionService, IDisposable {
  static functionDescription2Info(fn: FunctionDescription): IFunctionInfo {
    return {
      functionName: fn.name,
      aliasFunctionName: undefined,
      functionType: fn.type,
      description: fn.description,
      abstract: fn.abstract,
      functionParameter: fn.params.map(DescriptionService.functionParameter2Param)
    };
  }

  private static functionParameter2Param(param: FunctionParameter): IFunctionParam {
    return {
      name: param.name,
      detail: param.detail,
      example: param.example,
      require: 1,
      repeat: 0   // not repeatable (only one argument)
    }
  }

  private functions: Map<IFunctionNames, IFunctionInfo> = new Map();

  dispose() {}

  getDescriptions = () => this.functions;

  getFunctionInfo(searchText: string) {
    return this.getDescriptions().get(searchText.toLowerCase());
  }

  getSearchListByName(searchText: string): ISearchItem[] {
    const normalSearchText = searchText.toLowerCase();
    return Array.from(this.getDescriptions().values())
      .filter(fn => fn.functionName.includes(normalSearchText)
                 || fn.aliasFunctionName?.includes(normalSearchText))
      .map(fn => ({name: fn.functionName, desc: fn.description}));
  }

  getSearchListByNameFirstLetter(searchText: string): ISearchItem[] {
    const normalSearchText = searchText.toLowerCase();
    return Array.from(this.functions.values())
      .filter(fn => fn.functionName.startsWith(normalSearchText)
                 || fn.aliasFunctionName?.startsWith(normalSearchText))
      .map(fn => ({name: fn.functionName, desc: fn.description}));
  }

  getSearchListByType(type: number): ISearchItem[] {
    return Array.from(this.functions.values())
      .filter(fn => fn.functionType === type)
      .map(fn => ({name: fn.functionName, desc: fn.description}));
  }

  hasFunction(searchText: string) {
    return this.getFunctionInfo(searchText.toLowerCase()) !== undefined;
  }

  hasDescription(name: string): boolean {
    return this.functions.has(name.toLowerCase());
  }

  hasDefinedNameDescription(name: string): boolean {
    return this.functions.has(name.toLowerCase());
  }

  isFormulaDefinedName(name: string): boolean {
    return this.functions.has(name.toLowerCase());
  }

  registerDescriptions(functionList: IFunctionInfo[]) {
    functionList.forEach(fn =>
      this.functions.set(fn.functionName, fn)
    );

    return toDisposable(() => {
      this.unregisterDescriptions(functionList.map(fn => fn.functionName));
    })
  }

  unregisterDescriptions(functionNames: string[]) {
    functionNames.forEach(functionName =>
      this.functions.delete(functionName)
    )
  }
}
