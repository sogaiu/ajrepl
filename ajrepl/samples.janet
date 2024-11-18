# XXX: container form to work-around ajrepl-send-expression-at-point bug.
#      commenting out the following may make the aforementioned function
#      fail to work for :keyword below.
(def b 2)

# forward-sexp works in both janet-mode and janet-ts-mode if
# starting at the left-most position

:keyword

12123213

"hello there (!) chaps"

# forward-sexp does not work for any of these with janet-mode
# forward-sexp works for all of these with janet-ts-mode
~,|(+ 1 2)
~,| (+ 1 2)
~ ,| (+ 1 2)
~ , | (+ 1 2)

# forward-sexp does not work with janet-mode
# forward-sexp works with janet-ts-mode
@{:a 1}

# forward-sexp starting at the beginning of the line comment on the
# following line results in point being after some-symbol for both
# janet-mode and janet-ts-mode

# just some line comment
janet/build

# XXX: leading @ mark
@{:a [1 2]
  :b {:x 2
      :y [2 "hello there (mate)"]}}

# XXX: this one is not handled correctly due to the space(s)
# XXX: note: the leading quote is for preventing compilation errors
'(' ' '[:a :b :c])

# XXX: note: the leading quote is for preventing compilation errors
'(''a ' ''b '(x y z))

(defn a
  [x]
  (+ x 1))

(comment

  (+ 1 1)
  # =>
  2

  )
