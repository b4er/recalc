const tsPlugin = require("@typescript-eslint/eslint-plugin");

const files = ["**/*.ts", "**/*.tsx"];

module.exports = [
  {
    files,
    linterOptions: {
      reportUnusedDisableDirectives: true,
    },
    languageOptions: {
      parser: require("@typescript-eslint/parser"),
    },
    plugins: {
      "@typescript-eslint": tsPlugin,
    },
    rules: {
      "@typescript-eslint/no-unused-vars": [
        "error",
        {
          "argsIgnorePattern": "^_",
          // "varsIgnorePattern": "^_",
          // "caughtErrorsIgnorePattern": "^_"
        }
      ]
    }
  },
  { files, rules: tsPlugin.configs.recommended.rules },
  {
    ignores: ["**/dist/**/*.js", "src/common/messages.d.ts"]
  },
  {
    rules: {
      "@typescript-eslint/no-require-imports": 0
    }
  }
];
