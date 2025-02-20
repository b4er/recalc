/** Data passed to index.js  */

// as specified in package.json
declare const config: {
  locale: "EN_US" | "ZH_CN";
  serverUri: string;
  logLevel: string;
};

declare const data: SheetDocument;
