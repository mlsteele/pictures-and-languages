;;; Collection of reuseable logo language procedures.
;;;
;;; This is a collection of logo procedures which are useful.
;;; They are loaded into the logo REPL environment by default.

(to (ngon sides size)
  (repeat sides
    (fd size)
    (rt (/ 360 sides))))

(to (triangle size)
  (ngon 3 size))

(to (square size)
  (ngon 4 size))

(to (pentagon size)
  (ngon 5 size))
