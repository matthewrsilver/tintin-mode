readme:
	emacs -batch -l ~/.emacs.d/el2markdown/el2markdown.el tintin-mode.el -f el2markdown-write-readme

test:
	emacs -batch -l ert -l tests/test-tintin-mode-files.el -f ert-run-tests-batch-and-exit