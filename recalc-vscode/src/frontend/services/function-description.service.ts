import { IDisposable } from "@univerjs/core";
import { IFunctionNames, IFunctionInfo, FunctionType } from "@univerjs/engine-formula";
import { IDescriptionService, ISearchItem } from "@univerjs/sheets-formula";

export class DescriptionService implements IDescriptionService, IDisposable {
  private functions: Map<IFunctionNames, IFunctionInfo> = new Map([
    ["not", {
      functionName: "not",
      aliasFunctionName: undefined,
      functionType: FunctionType.Logical,
      description: "Returns TRUE if the argument is FALSE, and FALSE if the argument is TRUE.",
      abstract: "Logical negation.",
      functionParameter: [
          {
              name: "logical",
              detail: "A boolean value or expression to negate.",
              example: "not(TRUE) → FALSE",
              require: 1,
              repeat: 0   // not repeatable (only one argument)
          }
      ]
    },
    ]
  ]);

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

  registerDescriptions = (_functionList: IFunctionInfo[]) =>
    ({dispose: () => {}});

  unregisterDescriptions = (_functionNames: string[]) => {};
}
