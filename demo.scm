;;; Demos given during presentation.
;;; See doc/presentation.pdf for the slides

(load "load")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logo Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Draw from REPL input.
(define ur (logo:canvas:ur (logo:repl)))
;;; Into REPL:
;;; Here's how to draw a dotted line.
(to (dotted-line length)
  (repeat (/ length 10)
    (pen-up)
    (fd 5)
    (pen-down)
    (fd 5)))
;;; Here's how to draw a colorific curvy shape.
(to (curve)
  (repeat 10
    (repeat 2
      (color "RED")
      (dotted-line 10)
      (color "BLUE")
      (dotted-line 5)
      (rt 10))))
;;; Yeah.. Do that a bunch of times.
(repeat 10
  (curve)
  (pen-up)
  (rt 180)
  (fd 100)
  (pen-down)
  (rt 180))
(commit) ;;; terminates the repl and returns the canvas
(draw ur) ;; Draw using the X backend


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hybrid Example 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; No interpreter here, Scheme is the base language.
(define (castle-walls)
  (mirror 90 (lambda _
    (square)
    (translate -0.6 1)
    (scale 0.4)
    ;; guard makes this recursive call auto-stop
    (guard castle-walls))))

(hybrid-reset!) ;; Clear the canvas
;; Draw a few of them
(mirror 90 (lambda _
  (repeat 4 (lambda _
    (castle-walls)
    (translate 2 0)))))

(draw *ur*) ;; Draw using the X backend


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hybrid Example 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %color-cycle
  (let ((colors '("red" "green" "blue" "purple" "orange")))
    (set-cdr! (last-pair colors) colors)
    colors))
;;; This is a normal scheme procedure.
(define (color-cycle!)
  (color! (car %color-cycle))
  (set! %color-cycle (cdr %color-cycle))
  'ok)

;;; This function in a more Context Free-like style.
(define (little-tree #!optional depth)
  (if (default-object? depth) (set! depth 1))
  (square 10)
  (if (= (random (ceiling (/ depth 2))) 0)
    (save-excursion (lambda _
      (scale 0.8)
      (translate 10 30 (lambda _
        (guard little-tree (+ depth 1))))
      (translate -20 30 (lambda _
        (guard little-tree (+ depth 1))))))))

;;; This procedure is written more like Logo.
;;; Draw an n-sided polygon with a decoration at the corners.
(define (ngon sides size decoration)
  (repeat sides (lambda _
    (rotate -90 (lambda _
      (decoration)))
    (line! 0 0 0 size)
    (forward size)
    (color! "red")
    (rotate (/ 360 sides)))))

(hybrid-reset!) ;; Clear the canvas
;;; Twelve-sided polygon decorated with little trees.
(ngon 12 100 (lambda _
  (color-cycle!)
  (little-tree)))
(draw *ur*)
