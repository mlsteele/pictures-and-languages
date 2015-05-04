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
feature = line | color
line = ('line x1 y1 x2 y2)
color = color "red|green|blue|..."
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
(define (ur-translate ur dx dy)
  (define (tx x) (+ x dx))
  (define (ty x) (+ x dy))
  (map (lambda (ele)
    (case (car ele)
      ((line) (cons 'line
                    (zip-apply (list tx ty tx ty)
                               (cdr ele))))
      ((point) (cons 'point (zip-apply (list tx ty) (cdr ele))))
      ((color) ele)
      (else (error "ur-translate does not recognize ur element" ele))))
    ur))

#| Test Cases
(define u
  '((line 0 0 10 100)
    (line 10 100 100 100)
    (line 100 100 10 0)))
(pp (ur-translate u 5 5))
(pp (ur-translate u -10 -10))
|#

;;; Scale a UR by sx in x
;;;           and sy in y
;;; Scale relative to the origin.
(define (ur-scale ur sx sy)
  (define (tx x) (* x sx))
  (define (ty x) (* x sy))
  (map (lambda (ele)
    (case (car ele)
      ((line) (cons 'line
                    (zip-apply (list tx ty tx ty)
                               (cdr ele))))
      ((point) (cons 'point (zip-apply (list tx ty) (cdr ele))))
      ((color) ele)
      (else (error "ur-scale does not recognize ur element" ele))))
    ur))

#| Test Cases
(define u
  '((line 0 0 10 100)
    (line 10 100 100 100)
    (line 100 100 10 0)))
(pp (ur-scale u 1 1))
(pp (ur-scale u 2 2))
(pp (ur-scale (ur-translate u -10 -10) 2 2))
|#

;;; Rescale from 1:1 width:height to ratio.
(define (ur-aspect-ratio ur ratio)
  (ur-scale ur 1 ratio))

(define (ur-fit-for-draw ur width height)
  (let* ((ur ur)
         ;; put corner at (0,0)
         (ur (ur-zero-corner ur))
         ;; fit into box of (0,0) to (width, height)
         (ur (ur-fit-box ur width height))
         ;; move from pixel to device coordinates
         (ur (ur-scale ur (/ 2 width) (/ 2 height)))
         (ur (ur-translate ur -1 -1))
         ;; add some padding
         (ur (ur-scale ur .8 .8))
         (ur (ur-translate ur .05 .05)))
    ur))

(define (ur-fit-for-svg ur width height)
  (let* ((ur ur)
         ;; put corner at (0,0)
         (ur (ur-zero-corner ur))
         ;; fit into box of (0,0) to (width, height)
         (ur (ur-fit-box ur width height)))
    ur))

;;; Take any ur and put its lower left corner at (0,0)
(define (ur-zero-corner ur)
  (ur-bounds ur (lambda (x-min y-min x-max y-max)
    (ur-translate ur (- x-min) (- y-min)))))

;;; Take a zero-cornered ur and fit it into a unit box.
(define (ur-unit-scale ur)
  (ur-bounds ur (lambda (x-min y-min x-max y-max)
    (let* ((max-dim (max x-max y-max))
           (factor (/ 1 max-dim)))
      (ur-scale ur factor factor)))))

;;; Take a zero-cornered ur and fit it into a width:height box.
(define (ur-fit-box ur width height)
  (ur-bounds ur (lambda (x-min y-min x-max y-max)
    (let* ((aspect-ur  (/ x-max y-max))
           (aspect-box (/ width height))
           (factor (if (> aspect-ur aspect-box)
                       (/ width x-max)
                       (/ height y-max))))
      (ur-scale ur factor factor)))))

#| Test Cases
(begin
  (hybrid-reset!)
  (square 100)
  (translate -10 10)
  (square 80)
  (draw *ur*))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Analyzers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the bounds of a UR.
;;; Calls receiver on (x-min y-min x-max y-max)
;;; Note that this uses +/-1e100 as maximum detectable
;;; because I'm lazy and couldn't find an infinity value.
(define (ur-bounds ur receiver)
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
  (for-each$ ur (lambda (ele)
    (case (car ele)
      ((line) (zip-apply (list note-x note-y note-x note-y)
                         (cdr ele)))
      ((point) (zip-apply (list note-x note-y) (cdr ele)))
      ((color) 'nop)
      (else (error "ur-bounds does not recognize ur element" ele)))))
  (receiver x-min y-min x-max y-max))

#| Test Cases
(define u
  '((line 10 5 100 300)
    (line 5 10 200 100)))
(pp (ur-bounds u list)) ; (5 5 200 300)
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
