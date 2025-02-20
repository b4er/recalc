import React from "react";

import { ISidebarService, useDependency } from "@univerjs/ui";

export function useSidebar(openCallback: () => void, closeCallback: () => void) {
  const sidebarService = useDependency(ISidebarService);
  React.useEffect(() => {
    if (sidebarService.visible) {
      openCallback();
    } else {
      closeCallback();
    }
  }, [sidebarService.visible])
}

const noOp = () => {};

export function useSidebarOpen(callback: () => void) {
  useSidebar(callback, noOp)
}

export function useSidebarClose(callback: () => void) {
  useSidebar(noOp, callback)
}
