"use strict";

const { test } = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

function extension({ fail = false, serverPath = "chipi-lsp" } = {}) {
  const clients = [];
  const commands = new Map();
  const errors = [];
  let configurationChanged;
  class LanguageClient {
    constructor(id, name, server, options) {
      Object.assign(this, { id, name, server, options });
      clients.push(this);
    }
    async start() { if (fail) throw new Error("ENOENT"); this.started = true; }
    async dispose() { this.disposed = true; }
  }
  const vscode = {
    workspace: {
      getConfiguration: () => ({ get: () => serverPath }),
      onDidChangeConfiguration: (fn) => { configurationChanged = fn; return { dispose() {} }; },
    },
    window: { showErrorMessage: async (message) => { errors.push(message); } },
    commands: {
      registerCommand: (id, fn) => { commands.set(id, fn); return { dispose() {} }; },
      executeCommand: async () => {},
    },
  };
  const sandbox = { module: { exports: {} }, require: (name) => {
    if (name === "vscode") return vscode;
    if (name === "vscode-languageclient/node") return { LanguageClient, TransportKind: { stdio: 0 } };
    throw new Error(`Unexpected dependency ${name}`);
  } };
  vm.runInNewContext(fs.readFileSync(path.join(__dirname, "../extension.js"), "utf8"), sandbox);
  return { ...sandbox.module.exports, clients, commands, errors, configurationChanged: (...args) => configurationChanged(...args) };
}

test("starts a stdio server, handles restart, and stops on deactivation", async () => {
  const ext = extension({ serverPath: "/path with spaces/chipi-lsp" });
  const context = { subscriptions: [] };
  await ext.activate(context);
  assert.equal(context.subscriptions.length, 2);
  const first = ext.clients[0];
  assert.equal(first.server.command, "/path with spaces/chipi-lsp");
  assert.equal(first.server.args[0], "--stdio");
  assert.equal(first.options.documentSelector[1].scheme, "untitled");
  assert.ok(first.started);
  await ext.commands.get("chipi.restartServer")();
  assert.ok(first.disposed);
  assert.ok(ext.clients[1].started);
  await ext.deactivate();
  assert.ok(ext.clients[1].disposed);
});

test("reports an actionable error if the binary is missing", async () => {
  const ext = extension({ fail: true });
  await ext.activate({ subscriptions: [] });
  assert.match(ext.errors[0], /Install the server or set chipi.serverPath/);
  assert.ok(ext.clients[0].disposed);
  await ext.deactivate();
});

test("configuration changes restart the client", async () => {
  const ext = extension();
  await ext.activate({ subscriptions: [] });
  ext.configurationChanged({ affectsConfiguration: (key) => key === "chipi.serverPath" });
  await ext.deactivate();
  assert.equal(ext.clients.length, 2);
  assert.ok(ext.clients.every((client) => client.disposed));
});
