"use strict";

const path = require("node:path");
const { runTests } = require("@vscode/test-electron");

runTests({
  extensionDevelopmentPath: path.resolve(__dirname, ".."),
  extensionTestsPath: path.resolve(__dirname, "host.cjs"),
  vscodeExecutablePath: process.env.VSCODE_EXECUTABLE,
  launchArgs: ["--disable-gpu", "--no-sandbox", "--skip-welcome", "--skip-release-notes", "--disable-workspace-trust"],
}).catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
