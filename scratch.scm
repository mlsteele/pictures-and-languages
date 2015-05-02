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


;;; Now draw from repl input
(define u (logo:canvas:ur (logo:repl)))
;;; Into REPL
(to (dotted-line length)
  (repeat (/ length 10)
    (pen-up)
    (fd 5)
    (pen-down)
    (fd 5)))
(color "RED")
(dotted-line 50)
(rt 90)
(color "blue")
(dotted-line 50)
;;; (commit) terminates the repl and returns the canvas
(commit)

(draw (ur-scale 1/100 1/100 u))

;;; Save the drawing as an SVG.
(pp u)
(ur->svg-file (ur-translate 50 50 u) "out.svg")
|#
