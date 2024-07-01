# janet-delims

Some functions for working with delimiters in fragments of Janet
source code.

## Functions

Provided functions include:

* `close-delims` - given a fragment of Janet source code, tries [1] to
  return a "delimiter-closed" version of the fragment.

* `closing-delims` - given a fragment of Janet source code, tries to
  return an array of what characters could "close-up" the unclosed
  delimiters

Note that these functions only try to handle the simple case of a
fragment that is missing closing delimiters at the "end".

See below for concrete example usages,

## Usages

### close-delims


```janet
(close-delims "(")
# =>
"()"

(close-delims "'(")
# =>
"'()"

(close-delims "~(smile breathe")
# =>
"~(smile breathe)"

(close-delims "{:a 1\n:b 2")
# =>
"{:a 1\n:b 2}"

(close-delims "[1 2 3 5 8")
# =>
"[1 2 3 5 8]"

(close-delims "{:a 1 :b [:x :y")
# =>
"{:a 1 :b [:x :y]}"

(def maybe-code
  (string "(defn hi\n"
          "  [x]\n"
          "  (+ 3 (* 8\n"
          "          (- 2 1)"))

(close-delims maybe-code)
# =>
(string maybe-code ")))")
```

### closing-delims

```janet
(closing-delims "(defn a")
# =>
@[")"]

(closing-delims "(defn a)")
# =>
@[]

(closing-delims "(defn a))")
# =>
nil

(closing-delims "{:a 1")
# =>
@["}"]

(closing-delims "[:x :y")
# =>
@["]"]

(closing-delims
  (string "{:a 1\n"
          " :b"))
# =>
@["}"]

(closing-delims
  (string "(defn my-fn\n"
          "  [x]\n"
          "  (+ x 1"))
# =>
@[")" ")"]

(closing-delims `"nice string"`)
# =>
@[]

(closing-delims `"not quite a string`)
# =>
@[`"`]

(closing-delims
  (string "``\n"
          "  hello"))
# =>
@["`" "`"]

(closing-delims `("what is going on?)`)
# =>
@[`"` `)`]

(closing-delims "``hello``")
# =>
@[]

(closing-delims "``hello```")
# =>
@["`"]

(closing-delims "@`hello")
# =>
@["`"]

(closing-delims "1")
# =>
@[]

(closing-delims "")
# =>
@[]
```

## Why

[ajrepl](https://github.com/sogaiu/ajrepl) and
[janet-editor-elf](https://github.com/sogaiu/janet-editor-elf) use
code from `janet-delims` to implement functionality including:

* indenting the current line
* indenting a region
* determining an expression before the cursor
* wrapping an expression in another form
* finding the bounds of an expression associated with the cursor
  location

## Footnotes

[1] If the fragment ends with a line comment (without a trailing
newline), `close-delims` does not give a correct result, e.g.

```janet
(close-delims "[:a # hello")
# =>
"[:a # hello]"
```

Possibly if the result was like `[:a # hello\n]` it might be nicer.

The `closing-delims` function is available to obtain the necessary
delimiters for "closing things up" and thus one could do:

```janet
(defn my-close-delims
  [fragment]
  (string fragment "\n" ;(closing-delims fragment)))
```

`close-delims` is not implemented this way because it would interfere
with other cases (because of the extra newline) and in practice the
comment case has not surfaced in the context of editor use.

