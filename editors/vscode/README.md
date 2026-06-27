# chipi for VS Code

Syntax highlighting, snippets, and a `.chipi` file icon for the
[chipi](https://github.com/ioncodes/chipi) instruction-set DSL.

## Features

- TextMate grammar for `.chipi`: declarations, keywords, builtins, types, numbers, operators, and
  display-template placeholders (`{field}`, `{cond?a:b}`, `{x:sym}`).
- Snippets for the common blocks (`decoder`, `selector`, `operand`, `type`, `instr`, `fn`,
  `length`, `prefix`).
- A file icon for `.chipi` specs.

## Install from source

```bash
cd editors/vscode
npm install          # only needed for packaging
npx vsce package     # produces chipi-<version>.vsix
code --install-extension chipi-*.vsix
```

## Grammar tests

```bash
npm test             # validates the grammar and its token classes against the fixture
```

## License

MIT OR Apache-2.0.
