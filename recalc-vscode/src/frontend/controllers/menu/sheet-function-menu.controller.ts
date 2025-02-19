import { MenuItemType, RibbonStartGroup } from "@univerjs/ui";

import { SheetFunctionToggleOperation } from "../../commands/commands/sheet-function.command";
import { SheetFunctionIcon } from "../../views/components/SheetFunctionIcon";
import { SheetFunctionContainer } from "../../views/sheet-function/SheetFunctionContainer";


function SheetFunctionToggleMenuItemFactory() {
  return {
    id: SheetFunctionToggleOperation.id,
    type: MenuItemType.BUTTON,
    title: 'sheetFunctionEditor.title',
    tooltip: 'sheetFunctionEditor.tooltip',
    icon: 'SheetFunctionIcon',
  }
}

export const menu = {
  commands: [SheetFunctionToggleOperation],
  components: {
    SheetFunctionIcon: SheetFunctionIcon,
    SheetFunctionContainer: SheetFunctionContainer,
  },
  schema: {
    [RibbonStartGroup.FILE]: {
      [SheetFunctionToggleOperation.id]: {
          order: 0,
          menuItemFactory: SheetFunctionToggleMenuItemFactory,
      },
    }
  }
}
