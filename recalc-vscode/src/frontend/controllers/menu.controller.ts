import { Disposable, ICommand, ICommandService, Inject, Injector } from "@univerjs/core";
import { ComponentManager, IMenuManagerService, MenuSchemaType } from "@univerjs/ui";

import { menu as sheetFunctionMenu } from "./menu/sheet-function-menu.controller";

type Menu = {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  commands: ICommand<any, any>[];
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  components: { [key: string]: any };
  schema: MenuSchemaType;
}

export class MenuController extends Disposable {
  constructor(
    @Inject(Injector) private readonly _injector: Injector,
    @ICommandService private readonly _commandService: ICommandService,
    @IMenuManagerService private readonly _menuManagerService: IMenuManagerService,
    @Inject(ComponentManager) private readonly _componentManager: ComponentManager,
  ) {
    super();

    // declare all the menus
    const menus = [sheetFunctionMenu];

    this._initCommands(menus);
    this._registerComponents(menus);
    this._initMenus(menus);
  }

  private _initCommands(menus: Menu[]) {
    menus.forEach(menu =>
      Array.from(menu.commands).forEach(command =>
        this.disposeWithMe(this._commandService.registerCommand(command))
      )
    )
  }

  private _initMenus(menus: Menu[]) {
    menus.forEach(menu => this._menuManagerService.mergeMenu(menu.schema))
  }

  private _registerComponents(menus: Menu[]) {
    menus.forEach(menu =>
      Object.entries(menu.components).forEach(([name, component]) =>
        this.disposeWithMe(this._componentManager.register(name, component))
      )
    )
  }
}
