(import ./delims)

(def expr-grammar
  ~{:main (some :input)
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
                      (choice (set "0efnrtvz\"\\")
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

  (peg/match expr-grammar " ")
  # => '@[(:whitespace " " 0 1)]

  (peg/match expr-grammar "# hi there ")
  # => '@[(:comment " hi there " 0 11)]

  (peg/match expr-grammar "8")
  # => '@[(:number "8" 0 1)]

  (peg/match expr-grammar "true")
  # => '@[(:constant "true" 0 4)]

  (peg/match expr-grammar `@"buffalo?"`)
  # => '@[(:buffer "buffalo?" 0 11)]
  
  (peg/match expr-grammar `"himo!"`)
  # => '@[(:string "himo!" 0 7)]

  (peg/match expr-grammar "``himo!``")
  # => '@[(:long-string "``himo!``" 0 9)]

  (peg/match expr-grammar "@``snake?``")
  # => '@[(:long-buffer "``snake?``" 0 11)]

  (peg/match expr-grammar ":smile")
  # => '@[(:keyword ":smile" 0 6)]
  
  (peg/match expr-grammar "print")
  # => '@[(:symbol "print" 0 5)]

  (peg/match expr-grammar "@()")
  # => '@[(:array 0 3)]

  (peg/match expr-grammar "()")
  # => '@[(:tuple 0 2)]

  (deep=
    #
    (peg/match expr-grammar "(+ 1 1)")
    #
    '@[(:tuple
         (:symbol "+" 1 2) (:whitespace " " 2 3)
         (:number "1" 3 4) (:whitespace " " 4 5)
         (:number "1" 5 6)
         0 7)])
  # => true

  (deep=
    #
    (peg/match expr-grammar "|(+ 1 $)")
    #
    '@[(:fn
         (:tuple
           (:symbol "+" 2 3) (:whitespace " " 3 4)
           (:number "1" 4 5) (:whitespace " " 5 6)
           (:symbol "$" 6 7)
           1 8)
         0 8)])
  # => true

  (peg/match expr-grammar "~0")
  # => '@[(:quasiquote (:number "0" 1 2) 0 2)]
  
  (peg/match expr-grammar "':hi")
  # => '@[(:quote (:keyword ":hi" 1 4) 0 4)]

  (deep=
    #
    (peg/match expr-grammar "[2 3]")
    #
    '@[(:bracket-tuple
         (:number "2" 1 2) (:whitespace " " 2 3)
         (:number "3" 3 4)
         0 5)])
  # => true

  (deep=
    #
    (peg/match expr-grammar ";[8 9]")
    #
    '@[(:splice
         (:bracket-tuple
           (:number "8" 2 3) (:whitespace " " 3 4)
           (:number "9" 4 5)
           1 6)
         0 6)])
  # => true

  (deep=
    #
    (peg/match expr-grammar "@[:x :z]")
    #
    '@[(:bracket-array
         (:keyword ":x" 2 4) (:whitespace " " 4 5)
         (:keyword ":z" 5 7)
         0 8)])
  # => true

  (deep=
    #
    (peg/match expr-grammar "~,1")
    #
    '@[(:quasiquote
         (:unquote
           (:number "1" 2 3)
           1 3)
         0 3)])
  # => true

  (deep=
    #
    (peg/match expr-grammar "@{:a 1}")
    #
    '@[(:table
         (:keyword ":a" 2 4) (:whitespace " " 4 5)
         (:number "1" 5 6)
         0 7)])
  # => true

  (deep=
    #
    (peg/match expr-grammar (string "{:alpha 1\n"
                                    " :beta 2}"))
    #
    '@[(:struct
         (:keyword ":alpha" 1 7) (:whitespace " " 7 8)
         (:number "1" 8 9) (:whitespace "\n" 9 10)
         (:whitespace " " 10 11)
         (:keyword ":beta" 11 16) (:whitespace " " 16 17)
         (:number "2" 17 18)
         0 19)])
  # => true
  
  )

(defn ast
  [code]
  (->> code
       (peg/match expr-grammar)
       first))

(comment

  (deep=
    #
    (ast "(+ 1 1)")
    #
    '(:tuple
       (:symbol "+" 1 2) (:whitespace " " 2 3)
       (:number "1" 3 4) (:whitespace " " 4 5)
       (:number "1" 5 6)
       0 7))
  # => true

  )

(defn node-type
  [node]
  (first node))

(defn start
  [node]
  (first (slice node -3 -2)))

(comment

  (start (ast "(+ 1 1)"))
  # => 0

  )

(defn end
  [node]
  (first (slice node -2)))

(comment

  (end (ast "(+ 1 1)"))
  # => 7

  )

(defn content
  [node]
  (slice node 1 -3))

(defn find-expr
  [tree pos]
  (var ctxt @[tree])
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
              (array/push ctxt item))))
        (helper item))))
  #
  (helper tree)
  # XXX
  (eprintf "ctxt: %p" ctxt)
  (last ctxt))

(defn last-expr
  [maybe-code]
  # remove trailing whitespace
  (def trimmed
    (string/trimr maybe-code))
  (def target-pos
    (length trimmed))
  # determine target span
  (def lines
    (string/split "\n" trimmed))
  # add any missing delimiters
  (def delims
    (delims/missing-delims trimmed))
  (when (not (empty? delims))
    (array/push lines delims))
  # parse to get a tree
  (def new-region
    (string/join lines "\n"))
  (def tree
    (ast new-region))
  (def result
    (find-expr tree target-pos))
  (when result
    (string/slice trimmed (start result) (end result))))

(comment

  (last-expr "1")
  # => "1"

  (let [maybe-code
        (string "(defn hi\n"
                "  [x]\n"
                "  (+ 3 (* 8\n"
                "          (- 2 1)")]
    (last-expr maybe-code))
  # => "(- 2 1)"

  (let [maybe-code
        (string "'(defn hi\n"
                "  [x]\n"
                "  (+ 3 (* 8\n"
                "          (- 2 1)")]
    (last-expr maybe-code))
  # => "(- 2 1)"

  (let [maybe-code
        (string "~(defn hi\n"
                "  [x]\n"
                "  (+ 3 (* 8\n"
                "          (- 2 1)")]
    (last-expr maybe-code))
  # => "(- 2 1)"

  (let [maybe-code
        (string "(defn hi\n"
                "  [x]\n"
                "  (+ 3 (* 8\n"
                "          (- 2 1)\n"
                "      ")]
    (last-expr maybe-code))
  # => "(- 2 1)"
  
  (let [maybe-code
        (string "'(defn hi\n"
                "  [x]\n"
                "  (+ 3 (* 8\n"
                "          (- 2 1)\n"
                "      ")]
    (last-expr maybe-code))
  # => "(- 2 1)"

  (let [maybe-code
        (string "~(defn hi\n"
                "  [x]\n"
                "  (+ 3 (* 8\n"
                "          (- 2 1)\n"
                "      ")]
    (last-expr maybe-code))
  # => "(- 2 1)"

  (let [maybe-code (string "a")]
    (last-expr maybe-code))
  # => "a"

  (let [maybe-code (string "[:a :b]")]
    (last-expr maybe-code))
  # => "[:a :b]"

  (let [maybe-code (string "(defn missing-delims\n"
                           "  [fragment]\n"
                           "  (var missing @\"\")\n"
                           "  (def p (parser/new))")]
    (last-expr maybe-code))
  # => "(def p (parser/new))"

  (let [maybe-code (string "(def a 1")]
    (last-expr maybe-code))
  # => "1"

  )

(defn main
  [& args]
  (def expr
    (last-expr (file/read stdin :all)))
  (eprint "expr: " expr)
  (print expr))
