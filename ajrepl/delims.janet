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
  (var missing @"")
  (def p (parser/new))
  (parser/consume p fragment)
  # XXX: in another code base, had a problem w/ parser/eof, but...
  #      parser/eof is necessary for some backtick cases, e.g. not possible
  #      to tell if ``hello`` is complete, as it could be the beginning of
  #      ``hello```!``
  (try
    (parser/eof p)
    ([err]
      (eprintf "parser/eof returned error: %p" err)
      # early return indicating problem
      (break nil)))
  #
  (when-let [state (parser/state p)
             delims (state :delimiters)]
    (when (pos? (length delims))
      (each d (reverse delims)
        (case d
          (chr "(") (buffer/push-string missing ")")
          (chr "[") (buffer/push-string missing "]")
          (chr "{") (buffer/push-string missing "}")
          (chr `"`) (buffer/push-string missing `"`)
          (chr "`") (buffer/push-string missing "`")
          # XXX: should not happen
          (errorf "Unrecognized delimiter character: %s"
                  (string (buffer/push-byte @"" d)))))))
  missing)

(comment

  (missing-delims "(defn a))")
  # => nil

  (missing-delims "(defn a")
  # => @")"

  (missing-delims "{:a 1")
  # => @"}"

  (missing-delims "[:x :y")
  # => @"]"

  (missing-delims
    (string "{:a 1\n"
            " :b"))
  # => @"}"

  (missing-delims
    (string "(defn my-fn\n"
            "  [x]\n"
            "  (+ x 1"))
  # => @"))"

  (missing-delims `"nice string"`)
  # => @""

  (missing-delims `"not quite a string`)
  # => @`"`

  (missing-delims `("what is going on?)`)
  # => @`")`

  (missing-delims "``hello``")
  # => @""

  (missing-delims "``hello```")
  # => @"`"

  (missing-delims "1")
  # => @""

  (missing-delims "")
  # => @""

  (missing-delims
    (string "``\n"
            "  hello"))
  # => @"``"

  )

(defn close-delims
  [maybe-code]
  (def delims (missing-delims maybe-code))
  (string maybe-code delims))

(comment

  (close-delims "(")
  # => "()"

  (close-delims "'(")
  # => "'()"

  (close-delims "~(smile breathe")
  # => "~(smile breathe)"

  (close-delims "{:a 1\n:b 2")
  # => "{:a 1\n:b 2}"

  (close-delims "[1 2 3 5 8")
  # => "[1 2 3 5 8]"

  (close-delims "{:a 1 :b [:x :y")
  # => "{:a 1 :b [:x :y]}"

  (let [maybe-code
        (string "(defn hi\n"
                "  [x]\n"
                "  (+ 3 (* 8\n"
                "          (- 2 1)")]
    (deep=
      #
      (close-delims maybe-code)
      #
      (string maybe-code ")))")))
  # => true

  )
