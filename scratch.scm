;;; Scratchpad for uncategorized work

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "ghelper")
(load "helpers")
(load "logo")


#| Example of Logo Usage
;;; Make a new canvas
(define c (logo:canvas:new))

;;; Run a really simple program in the logo interpreter.
(logo:eval '() c '(rotate 10))
;;; See that the turtle changed its angle.
(pp (logo:canvas:turtle c))
;;; And another simple program
(logo:eval '() c '(fd 100))
(pp (logo:canvas:turtle c))
;;; This one added a line to the canvas!
(pp c)

;;; Convert the canvas into a uniform representation.
(define u (logo/canvas->uniform c))
;;; Use the backend to show the result.
(the-backend/display u)
;;; A graphics window should pop up and show a slanted line.
|#
