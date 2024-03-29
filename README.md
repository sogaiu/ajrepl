# ajrepl

A mode for interacting with a Janet REPL

## Setup

How to set things up varies a bit depending on how one manages one's
Emacs, e.g. straight.el, Doom, etc.  What's common to all situations
is likely:

* Ensure [janet-ts-mode](https://github.com/sogaiu/janet-ts-mode) or
  `janet-mode` is installed and configured.  (If using `janet-mode`,
  replace `janet-ts-mode` and `janet-ts-mode-hook` below with
  `janet-mode` and `janet-mode-hook` respectively.)

* Clone this repository.

### straight.el

* I add the following sort of thing to my `.emacs`-equivalent:
    ```elisp
    (straight-use-package
      '(ajrepl :host github
               :repo "sogaiu/ajrepl"
               :files ("*.el" "ajrepl")))

    (use-package ajrepl
      :straight t
      :config
      (add-hook 'janet-ts-mode-hook
                #'ajrepl-interaction-mode))
    ```

### Doom

* The following might work for Doom:
    ```elisp
    (package! ajrepl
      :recipe (:type git
               :host github
               :repo "sogaiu/ajrepl"
               :files (:defaults ("ajrepl/"
                                  "ajrepl/*"))))

    (use-package! ajrepl
      :after janet-ts-mode
      :config
      (add-hook 'janet-ts-mode-hook
                #'ajrepl-interaction-mode))
    ```

### Vanilla

* If you cloned to `~/src/ajrepl`, add the following to your
  `.emacs`-equivalent:
    ```elisp
    (add-to-list 'load-path
                 (expand-file-name "~/src/ajrepl"))

    (require 'ajrepl)

    (add-hook 'janet-ts-mode-hook
              #'ajrepl-interaction-mode)
    ```

### package.el

* Sorry, no support for that.  The Vanilla instructions should work
  though.

## Usage

0. Open a Janet file

1. Start an interactive REPL for Janet by `M-x ajrepl`.

    A buffer for a Janet repl should appear.

2. Various forms of sending things to the REPL should be possible, e.g.

    * Send buffer (`C-c C-b`)
    * Send expression at point (`C-x C-e`)
    * Send expression upscoped (`C-c C-u`) - see below for details
    * Send region (`C-c C-r`)

    If Emacs' menus are enabled, there should be a `Ajrepl` menu
    which shows the above (and other) commands.

## Misc Notes

`ajrepl-send-expression-upscoped` can be handy when evaluating forms
such as `(import ...)` which might otherwise lead to output one might
not be interested in.

## Experimental Commands

There's also a file named `ajrepl-experiment.el` that contains some
experimental (aka "may not stick around") functionality.

If the file is required, it should add some additional things to the
`Ajrepl` menu.

## Issues

* The REPL buffer gets very messy due to prompt information from Janet.
  I don't know if there is a good way to address this without using
  netrepl.
