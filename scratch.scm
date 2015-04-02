;;; Scratchpad for miscellaneous work

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "ghelper")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pi (* 4 (atan 1.0)))

(define (degrees->rads degrees)
  (* 2 PI (/ degrees 360.)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic pretty printer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pp
  (make-generic-operator 1 'pp
                         (environment-lookup system-global-environment 'pp)))

(defhandler pp
  (lambda _ (display "<null>"))
  null?)

#| Example
(pp '()) ; <null>
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Turtle Type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <logo/turtle>
    (%logo/turtle/new pos angle pendown)
    logo/turtle?
  (pos     logo/turtle/pos     logo/turtle/set-pos!)
  (angle   logo/turtle/angle   logo/turtle/set-angle!)
  (pendown logo/turtle/pendown logo/turtle/set-pendown!))

(define (logo/turtle/new)
  (%logo/turtle/new '(0 0) 0 #t))

(defhandler pp
  (lambda (turtle)
    (pp (list 'turtle
              (logo/turtle/pos turtle)
              (logo/turtle/angle turtle)
              (logo/turtle/pendown turtle))))
  logo/turtle?)

(define (logo/turtle/rotate turtle angle)
  (logo/turtle/set-angle! turtle
    (modulo (+ angle
               (logo/turtle/angle turtle))
            360)))

(define (logo/turtle/forward turtle distance)
  (let* ((pos (logo/turtle/pos turtle))
         (x (car pos))
         (y (cadr pos))
         (angle (logo/turtle/angle turtle))
         (rads (degrees->rads angle)))
     (logo/turtle/set-pos! turtle
       (list (+ x (* distance (cos rads)))
             (+ y (* distance (sin rads)))))))

#| Example
(define t (logo/turtle/new))
(pp t)
(logo/turtle/rotate t 90)
(pp t)
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Canvas Type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <logo/canvas>
    (%logo/canvas/new turtle lines)
    logo/canvas?
  (turtle logo/canvas/turtle)
  (lines  logo/canvas/lines))

(define (logo/canvas/new)
  (%logo/canvas/new (logo/turtle/new) '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Logo Programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define example-tosq
  '(to (square size)
     (repeat 4
       (fd size)
       (rt 90))))

(define example-callsq
  '(square 100))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logo Language Recognizers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define logo/repeat?
  (match->combinators `(repeat (? ,logo/numexpr?) (?:+ (? ,logo/stmt?)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logo Language Evaluators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define logo/eval
  (make-generic-operator 3 'logo/eval
    (lambda args (apply logo/eval-default args))))

(define (logo/eval-default env canvas expr)
  (case (car expr)
    ((rotate rt) (logo/builtin-rotate env canvas expr))
    ((forward fd) (logo/builtin-forward env canvas expr))
    (else (error 'call-not-implemented))))

(define (logo/builtin-rotate env canvas expr)
  (let ((turtle (logo/canvas/turtle canvas))
        (angle (cadr expr)))
    (logo/turtle/rotate turtle angle)))

(define (logo/builtin-forward env canvas expr)
  (let ((turtle (logo/canvas/turtle canvas))
        (distance (cadr expr)))
    (logo/turtle/forward turtle distance)))

#| Example
(define c (logo/canvas/new))
(logo/eval '() c '(rotate 10))
(pp (logo/canvas/turtle c))
(logo/eval '() c '(fd 100))
(pp (logo/canvas/turtle c))
|#
