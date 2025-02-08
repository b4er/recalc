import * as esbuild from 'esbuild';
// import copy from 'esbuild-plugin-copy';

const defaultOptions = {
  bundle: true,
  loader: {
    '.tsx': 'tsx',
    '.ts': 'ts',
    '.js': 'js',
  },
  outdir: "dist",
  plugins: [
    // copy({assets: {from: 'media/*', to: '.'}}),
  ],
};

async function build(options) {
  await esbuild.build({...defaultOptions, ...options});
}

async function buildAll() {
  await build({
    entryPoints: ["src/frontend/index.ts"],
  });

  await build({
    entryPoints: ["src/extension/extension.ts"],
    platform: "node",
    external: ["vscode"],
  });

  await build({
    entryPoints: ["src/test/test.ts"],
    platform: "node",
    external: ["mocha"],
    plugins: [],
  });
}

buildAll().catch(err => {
  console.error(err);
  process.exit(1);
});
