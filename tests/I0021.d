// #run: (let ((a (progn (goto-char 170) (c-literal-limits))) (b (progn (goto-char 230) (c-literal-limits)))) (list a b))
// #out: ((167 . 172) (227 . 232))
auto x = `ab\`; // back-quoted string ends with a backslash
auto y = "c\""; // double-quoted string ends with an escaped "

// cc-mode bug:
// https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45988
// #condition: (or (version<= emacs-version "27") (version<= "27.2" emacs-version))
