"use strict";

const assert = require("node:assert/strict");
const path = require("node:path");
const vscode = require("vscode");

async function eventually(fn) {
  const deadline = Date.now() + 15000;
  while (Date.now() < deadline) {
    const result = await fn();
    if (result) return result;
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
  throw new Error("Timed out waiting for language server result");
}

async function run() {
  const binary = path.resolve(__dirname, "../../../target/debug", process.platform === "win32" ? "chipi-lsp.exe" : "chipi-lsp");
  await vscode.workspace.getConfiguration("chipi").update("serverPath", binary, vscode.ConfigurationTarget.Global);
  const extension = vscode.extensions.getExtension("ioncodes.chipi");
  assert.ok(extension, "extension is installed in development host");
  await extension.activate();
  const doc = await vscode.workspace.openTextDocument({
    language: "chipi",
    content: 'decoder D{width=8}\nselector op [7:4]\noperand reg=u4\nadd op=0 r:reg[3:0] | "add {r}"\n',
  });
  await vscode.window.showTextDocument(doc);
  const position = new vscode.Position(3, 12);
  const completions = await eventually(async () => {
    const value = await vscode.commands.executeCommand("vscode.executeCompletionItemProvider", doc.uri, position);
    return value?.items.some((item) => item.label === "reg") && value;
  });
  assert.ok(completions.items.some((item) => item.label === "fetch"));
  const definitions = await vscode.commands.executeCommand("vscode.executeDefinitionProvider", doc.uri, position);
  assert.equal(definitions[0].range.start.line, 2);
  const hovers = await vscode.commands.executeCommand("vscode.executeHoverProvider", doc.uri, position);
  assert.ok(hovers.length > 0);
  const symbols = await vscode.commands.executeCommand("vscode.executeDocumentSymbolProvider", doc.uri);
  assert.ok(symbols.some((symbol) => symbol.name === "add"));
  const edits = await vscode.commands.executeCommand("vscode.executeFormatDocumentProvider", doc.uri, { tabSize: 4, insertSpaces: true });
  let formatted = doc.getText();
  for (const edit of edits.sort((a, b) => doc.offsetAt(b.range.start) - doc.offsetAt(a.range.start))) {
    formatted = formatted.slice(0, doc.offsetAt(edit.range.start)) + edit.newText + formatted.slice(doc.offsetAt(edit.range.end));
  }
  assert.ok(formatted.includes("add op = 0"));
  const edit = new vscode.WorkspaceEdit();
  edit.replace(doc.uri, new vscode.Range(3, 11, 3, 14), "missing");
  await vscode.workspace.applyEdit(edit);
  await eventually(() => vscode.languages.getDiagnostics(doc.uri).some((diag) => diag.code === "UnknownName"));
  await vscode.commands.executeCommand("chipi.restartServer");
  await eventually(() => vscode.languages.getDiagnostics(doc.uri).some((diag) => diag.code === "UnknownName"));
  console.log("chipi VS Code host: autocomplete, definitions, hover, symbols, formatting, diagnostics and restart passed");
}

module.exports = { run };
