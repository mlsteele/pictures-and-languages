;;; Scratchpad for uncategorized work

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "load.scm")

#| Example of Logo Usage
;;; Make a new canvas
(define c (logo:canvas:new))
(define e (logo:make-env))

;;; Run a really simple program in the logo interpreter.
(logo:eval '(rotate 10) e c)
;;; See that the turtle changed its angle.
(pp (logo:canvas:turtle c))
;;; And another simple program
(logo:eval '(fd 100) e c)
(pp (logo:canvas:turtle c))
;;; This one added a line to the canvas!
(pp c)

;;; Convert the canvas into a uniform representation.
(define u (logo:canvas->uniform c))
(pp u)
;;; Use the backend to show the result.
(draw u)
(draw:close-graphics!)
;;; A graphics window should pop up and show a slanted line.


;;; Draw from REPL input.
(define ur (logo:canvas:ur (logo:repl)))
;;; Into REPL
(to (dotted-line length)
  (repeat (/ length 10)
    (pen-up)
    (fd 5)
    (pen-down)
    (fd 5)))
(to (curve)
  (repeat 10
    (repeat 2
      (color "RED")
      (dotted-line 10)
      (color "BLUE")
      (dotted-line 5)
      (rt 10))))
(to (flower)
  (repeat 10
    (curve)
    (pen-up)
    (rt 180)
    (fd 100)
    (pen-down)
    (rt 180)))

(to (diminishing-ngons sides size)
  (limit size 0.01)
  (ngon sides size)
  (diminishing-ngons sides (- size 10)))
(diminishing-ngons 5 250)
;;; (commit) terminates the repl and returns the canvas
(commit)
(draw ur) ;; Draw using the X backend

;;; Save the drawing as an SVG.
(pp u)
(ur->svg-file (ur-translate 50 50 u) "out.svg")
|#
