# Jed Northridge's Emacs Setup

This project documents how I get up and running with Emacs on a new
machine.

The project also describes my Emacs configuration.

## Background

I use Emacs 24+ on MacOS with the Emacs Starter Kit (ESK).

I get Emacs via brew, using something like `brew install emacs --cocoa`.

Once I have Emacs, I use the package management facilities to get the
ESK.

Once I've got Emacs and the ESK, it is now the case that starting
Emacs will in turn case the ESK to look in the directory
~/emacs.d/$USER and load up any files ending in .el that it sees
there.

While .el files are directly involved here, I don't author any of
these myself.

Instead, I create .org files that serve three purposes:

* The .org files contain documentation of my configuration.
* The .org files contain embedded elisp configuration.
* The .org files can be parsed and the elisp can be extracted from them.

So, there it is: I maintain my emacs configuration side-by-side with
its documentation in .org files, and then I extract these .org files
out into .el files and this is what Emacs actually uses at start up
time.
