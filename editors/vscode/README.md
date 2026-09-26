# chipi for VS Code

Language support for the [chipi](https://github.com/ioncodes/chipi) instruction-set
DSL. Open a `.chipi` file to get diagnostics, autocomplete, navigation and formatting.

## What it provides

- Errors and warnings from the chipi compiler, updated as you type.
- Completion for declarations, types, builtin functions and symbols in scope.
- Hover information and go-to-definition for declarations and local bindings.
- Find References for code identifiers in the current file.
- Document symbols, outline navigation and folding for multiline declarations.
- Document formatting, including Format on Save.
- Syntax highlighting, snippets and a `.chipi` file icon.

The server works on the open buffer, so unsaved edits are included. Documents are
independent: there is no workspace-wide index or cross-file resolution. References
cover code identifiers, not names embedded in assembly display strings. Rename,
code actions and signature help are not implemented yet.

Formatting normalizes indentation and token spacing. It preserves comments, string
contents, line breaks and the meaning of a spec. It follows the editor's tab size
and tabs/spaces setting. A document with a lexical error is left unchanged until
that error is fixed. Syntax errors can limit symbol navigation; completion still
offers keywords and builtins and attempts to recover surrounding declarations.

## Install

Install the language server from crates.io:

```sh
cargo install chipi-lsp --locked
```

The extension is installed separately. To build it from a chipi checkout, use
Node.js 22 or newer:

```sh
cd editors/vscode
npm ci
npm run package
code --install-extension chipi-1.0.0.vsix
```

This extension requires VS Code 1.82 or newer. It runs in the desktop or remote
extension host; install `chipi-lsp` in the same environment. The VSIX does not bundle
a platform-specific server executable.

The server is found through `PATH` by default. If VS Code does not inherit your
shell's Cargo path, set an absolute executable path:

```json
{
  "chipi.serverPath": "/home/you/.cargo/bin/chipi-lsp",
  "[chipi]": {
    "editor.defaultFormatter": "ioncodes.chipi",
    "editor.formatOnSave": true
  }
}
```

On Windows, select `chipi-lsp.exe`. Paths containing spaces work without adding
shell quotes. In an untrusted workspace, workspace settings cannot override the
server executable. Syntax highlighting remains available if the server is missing.

Use **chipi: Restart Language Server** from the command palette after updating the
binary. Changing `chipi.serverPath` restarts it automatically. Server logs appear
in the **chipi Language Server** output channel.

## Other editors

`chipi-lsp` speaks LSP over standard input and output. Configure your editor to run
`chipi-lsp --stdio` for the `chipi` filetype and associate `*.chipi` with that filetype.
It uses UTF-16 positions and supports incremental document synchronization. It does
not need a project configuration file or workspace root.

## Development

From the repository root, build the server with `cargo build -p chipi-lsp --locked`.
To install your checkout instead of the published server, use
`cargo install --path crates/chipi-lsp --locked`.

Then, in this directory:

```sh
npm ci
npm test          # grammar and extension lifecycle tests
npm run test:host # real VS Code integration tests
npm run package  # bundle the client and build a VSIX
```

The host tests download VS Code into `.vscode-test/` and use an isolated profile.
Use `xvfb-run -a npm run test:host` on headless Linux. Set `VSCODE_EXECUTABLE` to
reuse an installed VS Code executable. The server's stdio protocol and formatter
regressions run as part of `cargo test -p chipi-lsp`.

## License

MIT or Apache-2.0, your choice. See [LICENSE.md](LICENSE.md).
