(def env (make-env))

(defn my-get-prompt
  [_]
  "")

(comment

  (my-get-prompt nil)
  # =>
  ""

  )

(defn my-get-prompt
  [p]
  (def [line] (parser/where p))
  (if (empty? (parser/state p :delimiters))
    (string "repl:" line ":" "> ")
    ""))

(comment

  (my-get-prompt (parser/new))
  # =>
  "repl:1:> "

  (my-get-prompt (let [p (parser/new)]
                   (parser/consume p "(")
                   p))
  # =>
  ""

  )

(defn my-getstdin [prmpt buf _]
  (file/write stdout prmpt)
  (file/flush stdout)
  (file/read stdin :line buf))

(defn my-getchunk
  [buf p]
  # getline may not do what you want in some contexts (e.g. within emacs)
  (my-getstdin (my-get-prompt p) buf env))

(defn main
  [& argv]
  (repl my-getchunk nil env))
