import { Dependency, Inject, Injector, Plugin, UniverInstanceType } from '@univerjs/core';
import { IDescriptionService } from '@univerjs/sheets-formula';

import { HoverController } from './controllers/hover.controller';
import { MenuController } from './controllers/menu.controller';
import { MessageController } from './controllers/rpc.controller';

import { DescriptionService } from './services/function-description.service';
import { FunctionType } from '@univerjs/engine-formula';

export class RecalcPlugin extends Plugin {
  static override type = UniverInstanceType.UNIVER_SHEET;
  static override pluginName = 'SHEET_RECALC_PLUGIN';

  constructor(
    @Inject(Injector) override readonly _injector: Injector,
  ) {
    super();
  }

  override onStarting() {
    const adds: Dependency[] = [
      [MenuController],
      [HoverController],
      [MessageController],
    ];

    const overrides: Dependency[] = [
      [IDescriptionService, { useClass: DescriptionService }],
    ];

    // add dependencies
    adds.forEach(dep => this._injector.add(dep));
    overrides.forEach(dep => this._injector.replace(dep));
  }

  override onReady() {
    this._injector.get(MenuController);
    this._injector.get(HoverController);
    this._injector.get(MessageController);
    this._injector.get(IDescriptionService).registerDescriptions([
      DescriptionService.functionDescription2Info({
        name: "not",
        type: FunctionType.Logical,
        description: "Returns TRUE if the argument is FALSE, and FALSE if the argument is TRUE.",
        abstract: "Logical negation.",
        params: [
          {
            name: "logical",
            detail: "A boolean value or expression to negate.",
            example: "FALSE",
          }
        ]
      })
    ]);
  }
}
