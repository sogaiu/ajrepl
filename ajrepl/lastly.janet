#! /usr/bin/env janet

(comment import ./dalimit :prefix "")
(defn d/deprintf
  [fmt & args]
  (when (os/getenv "VERBOSE")
    (eprintf fmt ;args)))

(comment

  # example parser/state based on `{:a 1`
  @{:delimiters "{"
    :frames @[@{:args @[]
                :column 0
                :line 1
                :type :root}
              @{:args @[:a 1]
                :column 1
                :line 1
                :type :struct}]}

  )

(defn d/missing-delims
  [fragment]
  (var missing @[])
  (var start-pos nil)
  (var d-type nil)
  #
  (def p (parser/new))
  (parser/consume p fragment)
  # XXX: in another code base, had a problem w/ parser/eof, but...
  #      parser/eof is necessary for some backtick cases, e.g. not possible
  #      to tell if ``hello`` is complete, as it could be the beginning of
  #      ``hello```!``
  (try
    (parser/eof p)
    ([err]
      (d/deprintf "parser/eof returned error: %p" err)
      (d/deprintf "fragment was: %s" fragment)
      # early return indicating problem
      (break nil)))
  #
  (when-let [state (parser/state p)
             delims (state :delimiters)
             last-frame (last (state :frames))]
    (when (pos? (length delims))
      (set start-pos
           [(last-frame :line) (last-frame :column)])
      (set d-type
           (last-frame :type))
      (each d (reverse delims)
        (case d
          (chr "(") (array/push missing ")")
          (chr "[") (array/push missing "]")
          (chr "{") (array/push missing "}")
          (chr `"`) (array/push missing `"`)
          (chr "`") (array/push missing "`")
          # XXX: should not happen
          (errorf "Unrecognized delimiter character: %s"
                  (string (buffer/push-byte @"" d)))))))
  [missing start-pos d-type])

(comment

  (d/missing-delims "(defn a))")
  # =>
  nil

  (d/missing-delims "(defn a")
  # =>
  [@[")"] [1 1] :tuple]

  (d/missing-delims "{:a 1")
  # =>
  [@["}"] [1 1] :struct]

  (d/missing-delims "[:x :y")
  # =>
  [@["]"] [1 1] :tuple]

  (d/missing-delims
    (string "{:a 1\n"
            " :b"))
  # =>
  [@["}"] [1 1] :struct]

  (d/missing-delims
    (string "(defn my-fn\n"
            "  [x]\n"
            "  (+ x 1"))
  # =>
  [@[")" ")"] [3 3] :tuple]

  (d/missing-delims `"nice string"`)
  # =>
  [@[] nil nil]

  (d/missing-delims `"not quite a string`)
  # =>
  [@[`"`] [1 1] :string]

  (d/missing-delims `("what is going on?)`)
  # =>
  [@[`"` `)`] [1 2] :string]

  (d/missing-delims "``hello``")
  # =>
  [@[] nil nil]

  (d/missing-delims "``hello```")
  # =>
  [@["`"] [1 10] :string]

  (d/missing-delims "@`hello")
  # =>
  [@["`"] [1 2] :buffer]

  (d/missing-delims "1")
  # =>
  [@[] nil nil]

  (d/missing-delims "")
  # =>
  [@[] nil nil]

  (d/missing-delims
    (string "``\n"
            "  hello"))
  # =>
  [@["`" "`"] [1 1] :string]

  )

(defn d/closing-delims
  [fragment]
  (when-let [[delims _ _]
             (d/missing-delims fragment)]
    delims))

(comment

  (d/closing-delims "(defn a")
  # =>
  @[")"]

  (d/closing-delims "(defn a)")
  # =>
  @[]

  (d/closing-delims "(defn a))")
  # =>
  nil

  (d/closing-delims "{:a 1")
  # =>
  @["}"]

  (d/closing-delims "[:x :y")
  # =>
  @["]"]

  (d/closing-delims
    (string "{:a 1\n"
            " :b"))
  # =>
  @["}"]

  (d/closing-delims
    (string "(defn my-fn\n"
            "  [x]\n"
            "  (+ x 1"))
  # =>
  @[")" ")"]

  (d/closing-delims `"nice string"`)
  # =>
  @[]

  (d/closing-delims `"not quite a string`)
  # =>
  @[`"`]

  (d/closing-delims
    (string "``\n"
            "  hello"))
  # =>
  @["`" "`"]

  (d/closing-delims `("what is going on?)`)
  # =>
  @[`"` `)`]

  (d/closing-delims "``hello``")
  # =>
  @[]

  (d/closing-delims "``hello```")
  # =>
  @["`"]

  (d/closing-delims "@`hello")
  # =>
  @["`"]

  (d/closing-delims "1")
  # =>
  @[]

  (d/closing-delims "")
  # =>
  @[]

  )

(defn d/close-delims
  [maybe-code &opt try-harder?]
  (def delims
    (d/closing-delims maybe-code))
  (def cand (string maybe-code (string/join delims)))
  (when (not try-harder?)
    (break cand))
  #
  (def [ok? _] (protect (parse cand)))
  (when ok?
    (break cand))
  #
  (def new-cand (string maybe-code "\n" (string/join delims)))
  (def [new-ok? _] (protect (parse new-cand)))
  (assertf new-ok? "failed to close delims for: %n" maybe-code)
  #
  new-cand)

(comment

  (d/close-delims "(")
  # =>
  "()"

  (d/close-delims "'(")
  # =>
  "'()"

  (d/close-delims "~(smile breathe")
  # =>
  "~(smile breathe)"

  (d/close-delims "{:a 1\n:b 2")
  # =>
  "{:a 1\n:b 2}"

  (d/close-delims "[1 2 3 5 8")
  # =>
  "[1 2 3 5 8]"

  (d/close-delims "{:a 1 :b [:x :y")
  # =>
  "{:a 1 :b [:x :y]}"

  (def maybe-code
    (string "(defn hi\n"
            "  [x]\n"
            "  (+ 3 (* 8\n"
            "          (- 2 1)"))

  (d/close-delims maybe-code)
  # =>
  (string maybe-code ")))")

  (d/close-delims "[:a # hello")
  # =>
  "[:a # hello]"

  (d/close-delims "[:a # hello" :try-harder)
  # =>
  (string "[:a # hello\n"
          "]")

  )



(defn deprintf
  [fmt & args]
  (when (os/getenv "VERBOSE")
    (eprintf fmt ;args)))

(def expr-grammar
  ~{:main (cmt (sequence (capture (position))
                         (some :input)
                         (capture (position)))
               ,|[:code
                  ;(slice $& 2 -3)
                  (first $&) ;(slice $& -3 -2)])
    #
    :input (choice :non-form
                   :form)
    #
    :non-form (choice :whitespace
                      :comment)
    #
    :whitespace
    (cmt (capture (sequence (position)
                            (choice (some (set " \0\f\t\v"))
                                    (choice "\r\n"
                                            "\r"
                                            "\n"))
                            (position)))
         ,|[:whitespace (last $&) ;(slice $& 0 -2)])
    #
    :comment
    (cmt (sequence (position)
                   "#"
                   (capture (any (if-not (set "\r\n") 1)))
                   (position))
         ,|[:comment $1 $0 $2])
    #
    :form (choice :reader-macro
                  :collection
                  :literal)
    #
    :reader-macro (choice :fn
                          :quasiquote
                          :quote
                          :splice
                          :unquote)
    #
    :fn
    (cmt (capture (sequence (position)
                            "|"
                            (any :non-form)
                            :form
                            (position)))
         # $& is the remaining arguments
         ,|[:fn
            ;(slice $& 1 -3)
            ;(slice $& 0 1) ;(slice $& -3 -2)])
    #
    :quasiquote
    (cmt (capture (sequence (position)
                            "~"
                            (any :non-form)
                            :form
                            (position)))
         ,|[:quasiquote
            ;(slice $& 1 -3)
            ;(slice $& 0 1) ;(slice $& -3 -2)])
    #
    :quote
    (cmt (capture (sequence (position)
                            "'"
                            (any :non-form)
                            :form
                            (position)))
         ,|[:quote
            ;(slice $& 1 -3)
            ;(slice $& 0 1) ;(slice $& -3 -2)])
    #
    :splice
    (cmt (capture (sequence (position)
                            ";"
                            (any :non-form)
                            :form
                            (position)))
         ,|[:splice
            ;(slice $& 1 -3)
            ;(slice $& 0 1) ;(slice $& -3 -2)])
    #
    :unquote
    (cmt (capture (sequence (position)
                            ","
                            (any :non-form)
                            :form
                            (position)))
         ,|[:unquote
            ;(slice $& 1 -3)
            ;(slice $& 0 1) ;(slice $& -3 -2)])
    #
    :literal (choice :number
                     :constant
                     :buffer
                     :string
                     :long-buffer
                     :long-string
                     :keyword
                     :symbol)
    #
    :collection (choice :array
                        :bracket-array
                        :tuple
                        :bracket-tuple
                        :table
                        :struct)
    #
    :number
    (cmt (capture (sequence (position)
                            (drop (cmt
                                    (capture (some :name-char))
                                    ,scan-number))
                            (position)))
         ,|[:number (last $&) ;(slice $& 0 -2)])
    #
    :name-char (choice (range "09" "AZ" "az" "\x80\xFF")
                       (set "!$%&*+-./:<?=>@^_"))
    #
    :constant
    (cmt (capture (sequence (position)
                            (choice "false" "nil" "true")
                            (position)
                            (not :name-char)))
         ,|[:constant (last $&) ;(slice $& 0 -2)])
    #
    :buffer
    (cmt (sequence (position)
                   "@\""
                   (capture
                     (any (choice :escape
                                  (if-not "\"" 1))))
                   "\""
                   (position))
         ,|[:buffer $1 $0 $2])
    #
    :escape (sequence "\\"
                      (choice (set `"'0?\abefnrtvz`)
                              (sequence "x" [2 :hex])
                              (sequence "u" [4 :hex])
                              (sequence "U" [6 :hex])
                              (error (constant "bad escape"))))
    #
    :hex (range "09" "af" "AF")
    #
    :string
    (cmt (sequence (position)
                   "\""
                   (capture (any (choice :escape
                                         (if-not "\"" 1))))
                   "\""
                   (position))
         ,|[:string $1 $0 $2])
    # XXX: includes delimiters in representation portion
    :long-string
    (cmt (capture (sequence (position)
                            :long-bytes
                            (position)))
         ,|[:long-string (last $&) ;(slice $& 0 -2)])
    #
    :long-bytes {:main (drop (sequence :open
                                       (any (if-not :close 1))
                                       :close))
                 :open (capture :delim :n)
                 :delim (some "`")
                 :close (cmt (sequence (not (look -1 "`"))
                                       (backref :n)
                                       (capture :delim))
                             ,=)}
    # XXX: includes delimiters in representation portion
    :long-buffer
    (cmt (sequence (position)
                   "@"
                   (capture :long-bytes)
                   (position))
         ,|[:long-buffer $1 $0 $2])
    #
    :keyword
    (cmt (capture (sequence (position)
                            ":"
                            (any :name-char)
                            (position)))
         ,|[:keyword (last $&) ;(slice $& 0 -2)])
    #
    :symbol
    (cmt (capture (sequence (position)
                            (some :name-char)
                            (position)))
         ,|[:symbol (last $&) ;(slice $& 0 -2)])
    #
    :array
    (cmt (capture (sequence (position)
                            "@("
                            (any :input)
                            (choice ")"
                                    (error (constant "missing )")))
                            (position)))
         ,|[:array
            ;(slice $& 1 -3)
            ;(slice $& 0 1) ;(slice $& -3 -2)])
    #
    :tuple
    (cmt (capture (sequence (position)
                            "("
                            (any :input)
                            (choice ")"
                                    (error (constant "missing )")))
                            (position)))
         ,|[:tuple
            ;(slice $& 1 -3)
            ;(slice $& 0 1) ;(slice $& -3 -2)])
    #
    :bracket-array
    (cmt (capture (sequence (position)
                            "@["
                            (any :input)
                            (choice "]"
                                    (error (constant "missing ]")))
                            (position)))
         ,|[:bracket-array
            ;(slice $& 1 -3)
            ;(slice $& 0 1) ;(slice $& -3 -2)])
    #
    :bracket-tuple
    (cmt (capture (sequence (position)
                            "["
                            (any :input)
                            (choice "]"
                                    (error (constant "missing ]")))
                            (position)))
         ,|[:bracket-tuple
            ;(slice $& 1 -3)
            ;(slice $& 0 1) ;(slice $& -3 -2)])
    #
    :table
    (cmt (capture (sequence (position)
                            "@{"
                            (any :input)
                            (choice "}"
                                    (error (constant "missing }")))
                            (position)))
         ,|[:table
            ;(slice $& 1 -3)
            ;(slice $& 0 1) ;(slice $& -3 -2)])
    #
    :struct
    (cmt (capture (sequence (position)
                            "{"
                            (any :input)
                            (choice "}"
                                    (error (constant "missing }")))
                            (position)))
         ,|[:struct
            ;(slice $& 1 -3)
            ;(slice $& 0 1) ;(slice $& -3 -2)])
    })

(comment

  (peg/match expr-grammar `@"buffalo?"`)
  # =>
  '@[(:code (:buffer "buffalo?" 0 11) 0 11)]

  (peg/match expr-grammar `"himo!"`)
  # =>
  '@[(:code (:string "himo!" 0 7) 0 7)]

  (peg/match expr-grammar "``himo!``")
  # =>
  '@[(:code (:long-string "``himo!``" 0 9) 0 9)]

  (peg/match expr-grammar ":smile")
  # =>
  '@[(:code (:keyword ":smile" 0 6) 0 6)]

  (peg/match expr-grammar "@()")
  # =>
  '@[(:code (:array 0 3) 0 3)]

  (peg/match expr-grammar "(+ 1 1)")
  # =>
  '@[(:code
       (:tuple
         (:symbol "+" 1 2) (:whitespace " " 2 3)
         (:number "1" 3 4) (:whitespace " " 4 5)
         (:number "1" 5 6)
         0 7)
       0 7)]

  (peg/match expr-grammar "|(+ 1 $)")
  # =>
  '@[(:code
       (:fn
         (:tuple
           (:symbol "+" 2 3) (:whitespace " " 3 4)
           (:number "1" 4 5) (:whitespace " " 5 6)
           (:symbol "$" 6 7)
           1 8)
         0 8)
       0 8)]

  )

(defn ast
  [code]
  (first (peg/match expr-grammar code)))

(comment

  (ast "(+ 1 1)")
  # =>
  '(:code
     (:tuple
       (:symbol "+" 1 2) (:whitespace " " 2 3)
       (:number "1" 3 4) (:whitespace " " 4 5)
       (:number "1" 5 6)
       0 7)
     0 7)

  (ast "(+ 1 1) (- 2 1)")
  # =>
  '(:code
     (:tuple
       (:symbol "+" 1 2) (:whitespace " " 2 3)
       (:number "1" 3 4) (:whitespace " " 4 5)
       (:number "1" 5 6)
       0 7)
     (:whitespace " " 7 8)
     (:tuple
       (:symbol "-" 9 10) (:whitespace " " 10 11)
       (:number "2" 11 12) (:whitespace " " 12 13)
       (:number "1" 13 14)
       8 15)
     0 15)

  )

(defn node-type
  [node]
  (first node))

(defn start
  [node]
  (first (slice node -3 -2)))

(comment

  (start (ast "(+ 1 1)"))
  # =>
  0

  )

(defn end
  [node]
  (first (slice node -2)))

(comment

  (end (ast "(+ 1 1)"))
  # =>
  7

  )

(defn content
  [node]
  (slice node 1 -3))

(defn find-expr
  [tree pos]
  (var ctxt nil)
  (defn helper
    [node]
    (when (= :tuple (type node))
      (def inner (content node))
      #
      (each item inner
        (when (= :tuple (type item))
          (when (and (not= :whitespace
                           (node-type item))
                     (not= :comment
                           (node-type item)))
            (def end-pos (end item))
            (when (= pos end-pos)
              (set ctxt item)
              (break))))
        (helper item))))
  #
  (helper tree)
  (deprintf "ctxt: %p" ctxt)
  ctxt)

(defn last-expr
  [maybe-code]
  # remove trailing whitespace
  (def trimmed (string/trimr maybe-code))
  (def target-pos (length trimmed))
  # determine target span
  (def lines (string/split "\n" trimmed))
  # add any missing delimiters
  (def delims (d/closing-delims trimmed))
  (when (and delims (not (empty? delims)))
    (array/push lines (string/join delims)))
  # parse to get a tree
  (def new-region (string/join lines "\n"))
  (def tree (ast new-region))
  (def result (find-expr tree target-pos))
  (when result
    (let [s (start result)
          e (end result)]
      # XXX: not addressing underlying cause?
      (when (<= s e (length trimmed))
        (string/slice trimmed (start result) (end result))))))

(comment

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

  # regression test
  (last-expr "(+ 1 1) (- 2 1)")
  # =>
  "(- 2 1)"

  # regression test
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

  )

(defn main
  [& args]
  (def expr
    (last-expr (file/read stdin :all)))
  (deprintf "expr: %s" expr)
  (print expr))
