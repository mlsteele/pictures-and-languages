;;; Universal Representation
;;;
;;; Specification of a common format for specifying graphics features.
;;; As well as tools for manipulating them.
;;; A UR is an intermediate between frontend builders and backend renderers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format Specification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| Spec
A UR is a list of commands.
Each command's car indicates what it is.
So far only the 'line command type is supported.

UR = (feature*)
feature = line
line = ('line x1 y1 x2 y2)
|#

#| Examples
(define u
  '((line 0 0 10 100)
    (line 10 100 100 100)
    (line 100 100 10 0)))
(ur->svg-file u "out.svg")
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transformers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Translate a UR by (dx dy)
(define (ur-translate dx dy ur)
  (define (tx x) (+ x dx))
  (define (ty x) (+ x dy))
  (map (lambda (ele)
    (case (car ele)
      ((line) (cons 'line
                    (zip-apply (list tx ty tx ty)
                               (cdr ele))))
      ((color) ele)
      (else (error "ur-translate does not recognize ur element" ele))))
    ur))

#| Test Cases
(define u
  '((line 0 0 10 100)
    (line 10 100 100 100)
    (line 100 100 10 0)))
(pp (ur-translate 5 5 u))
(pp (ur-translate -10 -10 u))
|#

;;; Scale a UR by sx in x
;;;           and sy in y
;;; Scale relative to the origin.
(define (ur-scale sx sy ur)
  (define (tx x) (* x sx))
  (define (ty x) (* x sy))
  (map (lambda (ele)
    (case (car ele)
      ((line) (cons 'line
                    (zip-apply (list tx ty tx ty)
                               (cdr ele))))
      ((color) ele)
      (else (error "ur-scale does not recognize ur element" ele))))
    ur))

#| Test Cases
(define u
  '((line 0 0 10 100)
    (line 10 100 100 100)
    (line 100 100 10 0)))
(pp (ur-scale 1 1 u))
(pp (ur-scale 2 2 u))
(pp (ur-scale 2 2 (ur-translate -10 -10 u)))


|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Analyzers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the bounds of a UR.
;;; Returns a list of (x-min y-min x-max y-max)
;;; Note that this uses +/-1e100 as maximum detectable
;;; because I'm lazy and couldn't find an infinity value.
(define (ur-bounds ur)
  (ensure (> (length ur) 0)
          "Can only get bounds of non-empty UR.")
  (define x-min 1e100)
  (define y-min 1e100)
  (define x-max -1e100)
  (define y-max -1e100)
  (define (note-x x)
    (and (< x x-min) (set! x-min x))
    (and (> x x-max) (set! x-max x)))
  (define (note-y y)
    (and (< y y-min) (set! y-min y))
    (and (> y y-max) (set! y-max y)))
  (for-each (lambda (ele)
    (case (car ele)
      ((line) (zip-apply (list note-x note-y note-x note-y)
                         (cdr ele)))
      (else (error "ur-bounds does not recognize ur element" ele))))

            ur)
  (list x-min y-min x-max y-max))

#| Test Cases
(define u
  '((line 10 5 100 300)
    (line 5 10 200 100)))
(pp (ur-bounds u)) ; (5 5 200 300)
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ur:get-type ur)
  (assert (pair? ur))
  (assert (not (null? ur)))
  (car ur))

(define (ur:get-data ur)
  (assert (pair? ur))
  (cdr ur))

(define (ur:do-nothing)
  (ur:do-nothing-faster))
(define (ur:do-nothing-faster)
  (ur:do-nothing))
