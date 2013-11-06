#dotfiles

random dotfiles

##Emacs

###Cask

Cask is a package management system for Emacs which externalizes melpa package
dependencies, similar to Bundler.  The Cask file should be maintained in
version control.  I symlink my .emacs.d/Cask to the one here.

You can make sure Cask stays up to date with packages installed through the
emacs package system with Pallet.  Pallet supplies hooks into Emacs' package
commands (e.g., `package-install`) to keep the Cask file synchronized.

###init.el

With Cask, init.el is basically just initializing Cask and Pallet, and
whatever hooks and customizations you need.  I also symlink this into
.emacs.d

###site-lisp

This is where I put packages that are not available in melpa, or things that
I've hacked at.  This directory is also symlinked into .emacs.d


