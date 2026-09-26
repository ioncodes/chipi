"use strict";

const vscode = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");

let client;
let restarting = Promise.resolve();

async function start() {
  const command = vscode.workspace.getConfiguration("chipi").get("serverPath", "chipi-lsp").trim() || "chipi-lsp";
  const next = new LanguageClient(
    "chipi",
    "chipi Language Server",
    { command, args: ["--stdio"] },
    { documentSelector: [{ scheme: "file", language: "chipi" }, { scheme: "untitled", language: "chipi" }] },
  );
  client = next;
  try {
    await next.start();
  } catch (error) {
    client = undefined;
    console.error("chipi: language server startup failed", error);
    // The client library can reject dispose() after a failed initialization.
    await next.dispose().catch(() => {});
    void vscode.window.showErrorMessage(
      `Could not start chipi-lsp (${command}). Install the server or set chipi.serverPath to its executable. ${error.message}`,
      "Open Settings",
    ).then((action) => {
      if (action === "Open Settings") {
        return vscode.commands.executeCommand("workbench.action.openSettings", "chipi.serverPath");
      }
      return undefined;
    });
  }
}

function restart() {
  restarting = restarting.then(async () => {
    if (client) {
      await client.dispose();
      client = undefined;
    }
    await start();
  });
  return restarting;
}

async function activate(context) {
  context.subscriptions.push(
    vscode.commands.registerCommand("chipi.restartServer", restart),
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (event.affectsConfiguration("chipi.serverPath")) {
        void restart();
      }
    }),
  );
  await restart();
}

async function deactivate() {
  await restarting;
  if (client) {
    await client.dispose();
    client = undefined;
  }
}

module.exports = { activate, deactivate };
