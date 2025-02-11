import { Dependency, Inject, Injector, Plugin, UniverInstanceType } from '@univerjs/core';
import { ComponentManager } from '@univerjs/ui';
import { IRenderManagerService } from '@univerjs/engine-render';

import { MessageController } from './controllers/rpc.controller';
import { HoverController } from './controllers/hover.controller';

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
      [HoverController],
      [MessageController],
    ] as Dependency[]).forEach(dep => this._injector.add(dep))
  }

  override onReady() {
    ([
      [MessageController],
      [HoverController],
    ] as Dependency[]).forEach(ctrl => this._injector.get(ctrl[0]));
  }
}
