# Changelog

## 1.0.0

- chipi 1.0 syntax: dotted identity axes (`lda.dpx`) and `lda.*` dispatch patterns get their own
  scopes, `for`/`in`/`fetch` join the keywords, and `bbs_b{n}` name interpolation highlights the
  loop variable.
- New snippets: `for` (indexed-family expansion), `axis` (dotted leaf), `fetchm` (mode-dependent
  fetch width).
- Grammar tests cover the new token classes.

## 0.1.0

- Initial release: `.chipi` syntax highlighting, snippets, a file icon, and grammar tests.
