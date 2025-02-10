import { Dependency, Inject, Injector, Plugin, UniverInstanceType } from '@univerjs/core';
import { ComponentManager } from '@univerjs/ui';

import { RefSelectionsRenderService } from '@univerjs/sheets-formula-ui';
import { IRenderManagerService } from '@univerjs/engine-render';

import { MessageController } from './controllers/rpc.controller';

export class RecalcPlugin extends Plugin {
  static override type = UniverInstanceType.UNIVER_SHEET;
  static override pluginName = 'SHEET_RECALC_PLUGIN';

  constructor(
    @Inject(Injector) protected readonly _injector: Injector,
    @Inject(ComponentManager) private readonly _componentManager: ComponentManager,
    @IRenderManagerService private readonly _renderManagerService: IRenderManagerService,
  ) {
    super()
  }

  override onStarting() {
    ([
      [MessageController],
    ] as Dependency[]).forEach(dep => this._injector.add(dep))
  }

  override onReady() {
    this._injector.get(MessageController);
  }

  override onRendered(): void {
    ([
      [RefSelectionsRenderService],
     ] as Dependency[]).forEach((dep) =>
      // register and make sure things are disposed in time
      this.disposeWithMe(
        this._renderManagerService.registerRenderModule(UniverInstanceType.UNIVER_SHEET, dep)
      )
    );
  }
}
