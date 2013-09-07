# [Upcoming][upcoming]

* Create snippets for `begin_src` in org-mode
* Start using agenda within org-mode
* Start using slim-mode for `.slim` templates
* Start installing emacs with srgb
* Start using expand region
* Allow execution of sh scripts within active code blocks

# [v0.0.1][v0.0.1]

First version of a literately programmed emacs configuration.

Almost none of the associated elisp is mine. Instead, credit goes to
Phil Hagelberg, Magnar Sveen, Jim Weirich, and Sacha Chua. I attribute
their code throughout and inline.

* `org/appendix-a.org` describes how to install emacs and
  `emacs-setup`
* `org/appendix-b.org` describes how files in `org/*.org` are
  manipulated to form `emacs-setup.org`.
* `emacs-setup.org` can be tangled (`org-babel-tangle-file`) to create
  `emacs-setup.el`, a functional emacs configuration.
* `emacs-setup.org` can be exported to pdf via `org-export-as-pdf`
* `emacs-setup.org` can be exported to html via `org-export-as-html`

[v0.0.1]: https://github.com/jedcn/emacs-setup/tree/v0.0.1
[upcoming]: https://github.com/jedcn/emacs-setup/compare/v0.0.1...master
