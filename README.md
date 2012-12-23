# Jed Northridge's Emacs Setup

I don't understand emacs.

I don't know elisp.

I really don't understand computers.

But, I want to create some stuff using emacs.

This is how I set stuff up.

## Background

After almost 10 years of using emacs, I have a sinking feeling that I
should've started figuring out how to use this stuff about.. 10 years
ago.

This is a first (likely to fail) attempt to pay attention to how
things are working and set things up the way I'd like.

## How I got here

After watching the [Emacs Rocks](http://emacsrocks.com/) videos, I was
intrigued to see and use that setup.

I liked what I saw, and the fresh approach. This got me thinking about
really trying to figure things out.

Next up, I re-watched the [Peepcode on Emacs](https://peepcode.com/products/meet-emacs)

There were a few things I wanted to change, and as I went to customize
my setup, partially inspirted by the Meet Emacs Peepcode, I revisited
the [emacs-starter-kit](https://github.com/technomancy/emacs-starter-kit).

Combined with [Jim Weirich's setup](https://github.com/jimweirich/emacs-setup-esk), I figured this was a way forward.

## And So..

Here we go. The basic idea is that I setup Emacs as follows:

* Get Emacs 24 somehow. I go here: http://emacsformacosx.com/

* Start Emacs once. This will create a ~/.emacs.d directory

* git clone this repository. It can go anywhere on your computer, but
  you need the contents to appear to be at ~/.emacs.d/$USER. You could
  do this by cloning it and then moving the cloned directory to
  ~/.emacs.d/, and renaming it to be your $USER. You could also put it
  somewhere and link to it with ln -s.

* Then.. shut down emacs, and start it again! Wait for compilation.

* ta-da?

## Emacs Starter Kit, Emacs Rocks, Jim Weirich

This is just my minimalistic take on their stuff. Nothing original.