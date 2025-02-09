import { exec } from 'child_process';
import * as esbuild from 'esbuild';
import util from 'util';

const execPromise = util.promisify(exec);

const defaultOptions = {
  bundle: true,
  loader: {
    '.tsx': 'tsx',
    '.ts': 'ts',
    '.js': 'js',
  },
  outdir: "dist",
  plugins: [ ],
  tsconfig: './tsconfig.json'
};

async function build(options) {
  await esbuild.build({...defaultOptions, ...options});
}

async function buildAll() {
  await execPromise('tsc --noEmit');

  await build({
    entryPoints: ["src/frontend/index.ts"],
  });

  await build({
    entryPoints: ["src/extension/extension.ts"],
    platform: "node",
    external: ["vscode"],
  });

  await build({
    entryPoints: ["src/test/**/*.test.ts"],
    platform: "node",
    external: ["mocha"],
  });
}

buildAll().catch(err => {
  console.error(err);
  process.exit(1);
});
