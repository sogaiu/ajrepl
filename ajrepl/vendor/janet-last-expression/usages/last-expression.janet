(import ../janet-last-expression/last-expression :as le)

# expr-grammar
(comment

  (peg/match le/expr-grammar " ")
  # =>
  '@[(:code (:whitespace " " 0 1) 0 1)]

  (peg/match le/expr-grammar "# hi there ")
  # =>
  '@[(:code (:comment " hi there " 0 11) 0 11)]

  (peg/match le/expr-grammar "8")
  # =>
  '@[(:code (:number "8" 0 1) 0 1)]

  (peg/match le/expr-grammar "true")
  # =>
  '@[(:code (:constant "true" 0 4) 0 4)]

  (peg/match le/expr-grammar "~0")
  # =>
  '@[(:code (:quasiquote (:number "0" 1 2) 0 2) 0 2)]

  (peg/match le/expr-grammar "':hi")
  # =>
  '@[(:code (:quote (:keyword ":hi" 1 4) 0 4) 0 4)]

  (peg/match le/expr-grammar "@``snake?``")
  # =>
  '@[(:code (:long-buffer "``snake?``" 0 11) 0 11)]

  (peg/match le/expr-grammar "print")
  # =>
  '@[(:code (:symbol "print" 0 5) 0 5)]

  (peg/match le/expr-grammar "()")
  # =>
  '@[(:code (:tuple 0 2) 0 2)]

  (peg/match le/expr-grammar "[2 3]")
  # =>
  '@[(:code
       (:bracket-tuple
         (:number "2" 1 2) (:whitespace " " 2 3)
         (:number "3" 3 4)
         0 5)
       0 5)]

  (peg/match le/expr-grammar ";[8 9]")
  # =>
  '@[(:code
       (:splice
         (:bracket-tuple
           (:number "8" 2 3) (:whitespace " " 3 4)
           (:number "9" 4 5)
           1 6)
         0 6)
       0 6)]

  (peg/match le/expr-grammar "@[:x :z]")
  # =>
  '@[(:code
       (:bracket-array
         (:keyword ":x" 2 4) (:whitespace " " 4 5)
         (:keyword ":z" 5 7)
         0 8)
       0 8)]

  (peg/match le/expr-grammar "~,1")
  # =>
  '@[(:code
       (:quasiquote
         (:unquote
           (:number "1" 2 3)
           1 3)
         0 3)
       0 3)]

  (peg/match le/expr-grammar "@{:a 1}")
  # =>
  '@[(:code
       (:table
         (:keyword ":a" 2 4) (:whitespace " " 4 5)
         (:number "1" 5 6)
         0 7)
       0 7)]

  (peg/match le/expr-grammar (string "{:alpha 1\n"
                                     " :beta 2}"))
  # =>
  '@[(:code
       (:struct
         (:keyword ":alpha" 1 7) (:whitespace " " 7 8)
         (:number "1" 8 9) (:whitespace "\n" 9 10)
         (:whitespace " " 10 11)
         (:keyword ":beta" 11 16) (:whitespace " " 16 17)
         (:number "2" 17 18)
         0 19)
       0 19)]

  )

# last-expr
(comment

  (let [maybe-code (string "a")]
    (le/last-expr maybe-code))
  # =>
  "a"

  (let [maybe-code (string "[:a :b]")]
    (le/last-expr maybe-code))
  # =>
  "[:a :b]"

  )

