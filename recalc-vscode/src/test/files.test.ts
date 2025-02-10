import assert from "node:assert";
import 'mocha';

import { openFile, readJson } from "../files";

class Uri {
  path: string;

  constructor(filePath: string) {
    this.path = filePath;
  }

  toString() {
    return this.path;
  }
}

describe('readJson', function () {
  const sheetName = "Sheet 1";

  const sheetDocument = readJson(new Uri("test.rc"), JSON.stringify({
    sheetOrder: [sheetName],
    sheets: {[sheetName]: {} },
  }));

  const sheetId = sheetDocument.sheetOrder[0][0];

  it('should set the .sheets.[sheetId].id correctly', async () => {
    assert.strictEqual(sheetDocument.sheets[sheetId].id, sheetId);
  });

  it('should set the .sheets.[sheetId].name correctly', async () => {
    assert.strictEqual(sheetDocument.sheets[sheetId].name, sheetName);
  });

  it('example.rc is read correctly', async () => {
    const sheetDocument = openFile(new Uri("example.rc"));

    // sheetOrder: ["Sheet 2" "Sheet1"],

    const sheet1 = {
      name: sheetDocument.sheetOrder[1][1],
      id: sheetDocument.sheetOrder[1][0]
    };

    const sheet2 = {
      name: sheetDocument.sheetOrder[0][1],
      id: sheetDocument.sheetOrder[0][0]
    };

    assert.strictEqual("Sheet1", sheet1.name);
    assert.strictEqual("Sheet 2", sheet2.name);

    assert.deepEqual(
      sheetDocument.sheets[sheet1.id],
      {
        id: sheet1.id,
        name: sheet1.name,
      }
    );

    assert.deepEqual(
      sheetDocument.sheets[sheet2.id],
      {
        id: sheet2.id,
        name: sheet2.name,
        cellData: {
          "1": {"2": {"v": 12}}
        }
      }
    );
  })
});
