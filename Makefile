emacs ?= emacs
all: test

test: clean
	cask exec emacs -Q -batch -l d-mode-test.el -l d-mode.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -Q -batch -f batch-byte-compile d-mode.el

clean:
	rm -f d-mode.elc

.PHONY:	all test
