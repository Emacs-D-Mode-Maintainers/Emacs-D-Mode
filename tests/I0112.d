// #condition: (or (version<= emacs-version "27.1") (version<= "28.0" emacs-version))
// #run: (progn (goto-char (- (point-max) 3)) (read-only-mode -1) (insert "\"") (run-hooks 'post-command-hook) (insert "\"") (font-lock-ensure) (d-test-fontification))

void main()
{
	foo(
}
