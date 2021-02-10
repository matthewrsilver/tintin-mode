# Developing and Testing tintin-mode.el

This documentation provides general guidance for modifying and `tintin-mode`.

## Generating README.md

The README is converted from `tintin-mode.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown). Additional documentation is available there, but with `el2markdown` cloned inside `.emacs.d`, the following command does the trick:

```bash
emacs -batch -l ~/.emacs.d/el2markdown/el2markdown.el tintin-mode.el -f el2markdown-write-readme
```