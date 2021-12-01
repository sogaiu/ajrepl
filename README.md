# ajrepl

A mode for interacting with a Janet REPL

## Setup

How to set things up varies a bit depending on how one manages one's
Emacs, e.g. straight.el, Doom, etc.  What's common to all situations
is likely:

* Ensure a janet-mode is installed and configured.

* Clone this repository.

### straight.el

* I add the following sort of thing to my `.emacs`-equivalent:
    ```
    (straight-use-package
      '(ajrepl :host github
               :repo "sogaiu/ajrepl"
               :files ("*.el" "ajrepl")))

    (use-package ajrepl
      :straight t
      :config
      (add-hook 'janet-mode-hook
                #'ajrepl-interaction-mode))
    ```

### Doom

* The following might work for Doom:
    ```
    (package! ajrepl
      :recipe (:type git
               :host github
               :repo "sogaiu/ajrepl"
               :files (:defaults ("ajrepl/"
                                  "ajrepl/*"))))

    (use-package! ajrepl
      :after janet-mode
      :config
      (add-hook 'janet-mode-hook
                #'ajrepl-interaction-mode))
    ```

### Vanilla

* If you cloned to `~/src/ajrepl`, add the following to your
  `.emacs`-equivalent:
    ```
    (add-to-list 'load-path
                 (expand-file-name "~/src/ajrepl"))

    (add-hook 'janet-mode-hook
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
    * Send region (`C-c C-r`)

## Issues

* The REPL buffer gets very messy due to prompt information from Janet.
  I don't know if there is a good way to address this without using
  netrepl.
