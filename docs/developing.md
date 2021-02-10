# Developing and Testing tintin-mode.el

This documentation provides general guidance for modifying and `tintin-mode`.

## Testing

This code relies upon [faceup](https://github.com/Lindydancer/faceup) to run syntax highlighting tests through ert. To kick off all tests run the following from the repo's root directory:

```bash
$ make test
```

When modifying the `.tt` files that are used in these tests, it's important to regenerate the associated `.tt.faceup` files that pick up the highlighting and generate the appropriate faceup markup. To do this, open the file that is to be tested and use <kbd>M-x faceup-write-file RET</kbd> to generate the faceup file.

## Generating README.md

The README is converted from `tintin-mode.el` by [el2markdown](https://github.com/Lindydancer/el2markdown). Additional documentation is available there, but with `el2markdown` cloned inside `.emacs.d`, the following command does the trick:

```bash
$ make readme
```