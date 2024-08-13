# Emacs D Mode

[![Licence](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/workflows/Test/badge.svg)](https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/actions)
[![Coverage Status](https://coveralls.io/repos/github/Emacs-D-Mode-Maintainers/Emacs-D-Mode/badge.svg?branch=master)](https://coveralls.io/github/Emacs-D-Mode-Maintainers/Emacs-D-Mode?branch=master)
[![MELPA](https://melpa.org/packages/d-mode-badge.svg)](https://melpa.org/#/d-mode)
[![MELPA Stable](https://stable.melpa.org/packages/d-mode-badge.svg)](https://stable.melpa.org/#/d-mode)

An Emacs major mode for editing D code.

This mode is currently known to work with Emacs 25 and 26.
For best results, use Emacs 26.

The best way of installing this major mode, at least for Emacs 26, is to use the packaging system. Add MELPA
or MELPA Stable to the list of repositories to access this mode. For those who want only formal, tagged
releases use MELPA Stable:

```elisp
(require 'package)
(add-to-list 'package-archives
         '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
```

For those who want rolling releases as they happen use MELPA:

```elisp
(require 'package)
(add-to-list 'package-archives
         '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

and then use M-x package-list-packages to get to the package listing and install from there. MELPA tracks
this Git repository and updates relatively soon after each commit or formal release. For more detail on
setting up see [MELPA Getting Started](https://melpa.org/#/getting-started).

The master of all the material is the Git repository at
https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode .

This software is licenced under GNU General Public Licence version 3.
