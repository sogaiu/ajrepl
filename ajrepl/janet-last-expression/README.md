# janet-last-expression

Code for determining the last expression in a fragment [1] of Janet
source code.

## Demo

Invoking:

```
echo "(def a [:cat]" | janet janet-last-expression/last-expression.janet
```

should result in:

```
[:cat]
```

## Usages

Below are some usages of a provided function named `last-expr`.

```janet
(last-expr "1")
# =>
"1"

(last-expr "(def a 1")
# =>
"1"

(last-expr "(def a 1)")
# =>
"(def a 1)"

(last-expr "# a comment")
# =>
nil

(last-expr "     ")
# =>
nil

(last-expr ``
           (defn hi
             [x]
             (+ 3 (* 8
                     (- 2 1)
           ``)
# =>
"(- 2 1)"

(last-expr ``
           '(defn hi
             [x]
             (+ 3 (* 8
             (- 2 1)
            ``)
# =>
"(- 2 1)"

(last-expr (string "'(defn hi\n"
                   "  [x]\n"
                   "  (+ 3 (* 8\n"
                   "          (- 2 1)\n"
                   "      "))
# =>
"(- 2 1)"

(last-expr ``
           (defn missing-delims
             [fragment]
             (var missing @"")
             (def p (parser/new))
           ``)
# =>
"(def p (parser/new))"

(last-expr ``
           # a comment
           :hello
           ``)
# =>
":hello"

(last-expr "(+ 1 1) (- 2 1)")
# =>
"(- 2 1)"

(last-expr "'(+ 1 1)")
# =>
"'(+ 1 1)"

# XXX: perhaps prefer result to be :hello?
(last-expr ``
           :hello
           # a comment
           ``)
# =>
nil
```

## Users

The code is currently used via Emacs Lisp code in
[ajrepl](https://github.com/sogaiu/ajrepl).

## Footnotes

[1] The fragment of Janet source code does not have to be "complete"
(i.e. it can have unclosed delimiters but only at the very end), but
the code will assume the "incompleteness" is only at the tail end.

