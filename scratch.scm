;;; Scratchpad for uncategorized work

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "ghelper")
(load "matcher")
(load "helpers")
(load "uniform-representation")
(load "logo")
(load "draw")
(load "svg")


#| Example of Logo Usage
;;; Make a new canvas
(define c (logo:canvas:new))
(define e (make-root-top-level-environment))

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
(define u (logo:canvas->uniform (logo:repl)))
;;; Into REPL
(to (square size)
  (repeat 4
    (fd size)
    (rt 90)))
(square 20)
(square 30)
(rt 135)
(square 20)
;;; (commit) terminates the repl and returns the canvas
(commit)

(draw (ur-scale 1/100 1/100 u))

;;; Save the drawing as an SVG.
(pp u)
(ur->svg-file (ur-translate 50 50 u) "out.svg")
|#
