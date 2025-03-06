import { IUniverInstanceService, UniverInstanceType, Workbook, Worksheet } from "@univerjs/core";
import { useDependency, useObservable } from "@univerjs/ui";

// hook that returns the active worksheet
export function useActiveSheet(): Worksheet | null {
    const univerInstanceService = useDependency(IUniverInstanceService);
    const workbook = useObservable(() =>
      univerInstanceService.getCurrentTypeOfUnit$<Workbook>(UniverInstanceType.UNIVER_SHEET), undefined, undefined, []
    );

    return useObservable(workbook?.activeSheet$) ?? null;
}
