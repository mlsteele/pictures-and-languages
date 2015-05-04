;;; A graphical description language which is a hybrid of others.
;;; This language includes our favorite parts from other languages:
;;; This includes:
;;; - active transformation that applies to all commands
;;; - unwindable temporary transformations
;;; - automatic recursion base case detection for self-similar shapes
;;; - relative positioning and mirroring

;;; Active uniform representation of drawn stuf
(define *ur* '())
;;; Active transformation
(define *transformation* (m:identity))
;;; Continuation for when things get too small.
(define *limit-k* (lambda _
  (error "Draw limit reached at top level")))
;;; Smallest visible feature.
(define *limit-size* 0.005)

(define (hybrid-reset!)
  (set! *ur* '())
  (set! *transformation* (m:identity)))

(define (*ur*-add! ele)
  (set! *ur* (append *ur* (list ele))))

(define (*transformation*-apply! matrix)
  (set! *transformation* (m:* *transformation* matrix)))

(define (%transform x y)
  (m:*v *transformation* (list x y)))

#|
;;; Draw a point.
(define (point! #!optional x y)
 (if (any default-object? x y)
    (point! 0 0)
    (begin
      (error "not implemented")
  ... use transformation
  ... check if 1 is too small?
  ... ur)))
|#

(define (line! x1 y1 x2 y2)
  (let* ((p1 (%transform x1 y1))
         (p2 (%transform x2 y2))
         (x1 (car  p1))
         (y1 (cadr p1))
         (x2 (car  p2))
         (y2 (cadr p2)))
    (if (< (line-length x1 y1 x2 y2) *limit-size*)
      (*limit-k* 'limit-reached))
      (*ur*-add! `(line ,x1 ,y1 ,x2 ,y2))))

(define (color! color)
  (*ur*-add! `(color ,color)))

(define (save-excursion thunk)
  (let ((transformation *transformation*))
    (thunk)
    (set! *transformation* transformation)))

(define (%translate dx dy)
  (*transformation*-apply! (t:translate dx dy)))

(define (%rotate degrees)
  (*transformation*-apply! (t:rotate 'degrees degrees)))

(define (%scale x y)
  (*transformation*-apply! (t:scale x y)))

(define (%flip degrees)
  (*transformation*-apply! (t:flip 'degrees degrees)))

(define (translate dx dy #!optional thunk)
  (if (default-object? thunk)
    (%translate dx dy)
    (save-excursion (lambda _
      (%translate dx dy)
      (thunk)))))

(define (rotate degrees #!optional thunk)
  (if (default-object? thunk)
    (%rotate degrees)
    (save-excursion (lambda _
      (%rotate degrees)
      (thunk)))))

(define (forward dist #!optional thunk)
  (translate 0 dist thunk))

(define (scale x #!optional y thunk)
  (cond
   ((default-object? y)
    (scale x x))
   ((and (number? y) (default-object? thunk))
    (%scale x y))
   ((and (procedure? y) (default-object? thunk))
    (scale x x y))
   (else
    (save-excursion (lambda _
      (assert (number? y))
      (%scale x y)
      (thunk))))))

(define (flip degrees #!optional thunk)
  (if (default-object? thunk)
    (%flip degrees)
    (save-excursion (lambda _
      (%flip degrees)
      (thunk)))))

;;; Repeat a drawing task 'times.
;;; Each execution could change the transformation.
;;; Restore the transformation afterwards.
(define (repeat times thunk)
  (save-excursion (lambda _
    (let loop ((n times))
      (if (> n 0)
        (begin
          (thunk)
          (loop (- n 1))))))))

;;; Use this to call a recursive function safely.
(define (guard thunk)
  (call-with-current-continuation (lambda (k)
    (fluid-let ((*limit-k* k))
      (thunk)))))

(define (mirror degrees thunk)
  (save-excursion thunk)
  (flip degrees (lambda _
    (save-excursion thunk))))

(define (square #!optional size)
  (if (default-object? size)
    (square 1)
    (repeat 4 (lambda _
        (line! 0 0 0 size)
        (forward size)
        (rotate 90)))))

(define (%circle size resolution)
  (repeat resolution (lambda _
                       (rotate (/ 360 resolution))
                       (save-excursion (lambda _
                                         (translate 1 0)
                                         (line! 0 0 0 1))))))

(define (circle #!optional size resolution)
  (if (default-object? resolution)
      (circle size 16)
      (if (default-object? size)
          (circle 1 16)
          (%circle size resolution))))
