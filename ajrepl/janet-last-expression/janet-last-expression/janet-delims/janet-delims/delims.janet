(defn deprintf
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

(defn missing-delims
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
      (deprintf "parser/eof returned error: %p" err)
      (deprintf "fragment was: %s" fragment)
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

  (missing-delims "(defn a))")
  # =>
  nil

  (missing-delims "(defn a")
  # =>
  [@[")"] [1 1] :tuple]

  (missing-delims "{:a 1")
  # =>
  [@["}"] [1 1] :struct]

  (missing-delims "[:x :y")
  # =>
  [@["]"] [1 1] :tuple]

  (missing-delims
    (string "{:a 1\n"
            " :b"))
  # =>
  [@["}"] [1 1] :struct]

  (missing-delims
    (string "(defn my-fn\n"
            "  [x]\n"
            "  (+ x 1"))
  # =>
  [@[")" ")"] [3 3] :tuple]

  (missing-delims `"nice string"`)
  # =>
  [@[] nil nil]

  (missing-delims `"not quite a string`)
  # =>
  [@[`"`] [1 1] :string]

  (missing-delims `("what is going on?)`)
  # =>
  [@[`"` `)`] [1 2] :string]

  (missing-delims "``hello``")
  # =>
  [@[] nil nil]

  (missing-delims "``hello```")
  # =>
  [@["`"] [1 10] :string]

  (missing-delims "@`hello")
  # =>
  [@["`"] [1 2] :buffer]

  (missing-delims "1")
  # =>
  [@[] nil nil]

  (missing-delims "")
  # =>
  [@[] nil nil]

  (missing-delims
    (string "``\n"
            "  hello"))
  # =>
  [@["`" "`"] [1 1] :string]

  )

(defn closing-delims
  [fragment]
  (when-let [[delims _ _]
             (missing-delims fragment)]
    delims))

(comment

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

  )

(defn close-delims
  [maybe-code]
  (def delims
    (closing-delims maybe-code))
  (string maybe-code (string/join delims)))

(comment

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

  # XXX: would prefer different result, i.e. "[:a # hello\n]"
  (close-delims "[:a # hello")
  # =>
  "[:a # hello]"

  )

