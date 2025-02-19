import { CommandType, IAccessor, ICommand, IUniverInstanceService } from "@univerjs/core";
import { getSheetCommandTarget } from "@univerjs/sheets";
import { ISidebarService } from "@univerjs/ui";

export const SheetFunctionToggleOperation: ICommand = {
  id: `sheet-function.operation.toggle-ui`,
  type: CommandType.OPERATION,
  handler: async (accessor: IAccessor, params: {value: string}) => {
    const univerInstanceService = accessor.get(IUniverInstanceService);

    const target = getSheetCommandTarget(univerInstanceService);

    if (!target) {
      return false;
    }

    const sidebarService = accessor.get(ISidebarService);

    const close = params?.value !== undefined
      ? params.value : sidebarService.visible;

    if (close) {
      sidebarService.close()
    } else {
      sidebarService.open({
        id: 'SheetFunctionContainer',
        header: { title: 'sheetFunctionEditor.title' },
        children: { label: 'SheetFunctionContainer' },
        onClose: () => { if (sidebarService.visible) sidebarService.close(); },
        width: 480,
      })
    }

    return true;
  },
};
