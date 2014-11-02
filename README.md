# Emacs D Mode

An Emacs major mode for editing D code.

This mode is currently known to work with Emacs 24 and believed to work with Emacs 23.

The best way of installing this major mode, at least for Emacs 24, is to use the packaging system. Add
MELPA to the list of repositories:

    (require 'package)
    (add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (package-initialize)

and then use M-x package-list-package to get to the package listing and install from there. MELPA tracks
this Git repository and updates relatively soon after each commit, so there are unlikely to be any formal
releases of this major mode in the future.

The master of all the material is the Git repository at
https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode .

This software is licenced under GNU General Public Licence version 2.
