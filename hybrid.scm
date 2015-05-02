;;; A graphical description language which is a hybrid of others.
;;; This language includes our favorite parts from other languages:
;;; This includes:
;;; - active transformation that applies to all commands
;;; - unwindable temporary transformations
;;; - automatic recursion base case detection for self-similar shapes
;;; - relative positioning and mirroring

(define *ur* '())

(define *transformation* (m:identity))

(define (%transform x y)
  (m:*v *transformation* (list x y)))

(define (hybrid-reset!)
  (set! *ur* '())
  (set! *transformation* (m:identity)))

;;; Draw a point.
(define (point! #!optional x y)
  (if (any default-object? x y)
    (point! 0 0)
    (begin
      (error "not implemented")
  ... use transformation
  ... check if 1 is too small?
  ... ur)))

(define (line! x1 y1 x2 y2)
  (let* ((p1 (%transform x1 y1))
         (p2 (%transform x2 y2))
         (x1 (car  p1))
         (y1 (cadr p1))
         (x2 (car  p2))
         (y2 (cadr p2)))
    (if ?? boom!)
    (append! *ur* `(line ,x1 ,y1 ,x2 ,y2))))

(define (color! color)
  (append! *ur* `(color ,color)))

(define (save-excursion thunk)
  (let ((transformation *transformation*))
    (thunk)
    (set! *transformation* transformation)))

(define (%translate dx dy)
  ... do th heavy lifint...)

(define (%rotate degrees)
  ... do th heavy lifint...)

(define (translate dx dy #!optional thunk)
  (if (default-object? thunk)
    (save-excursion (lambda _
      (%translate dx dy)
      (thunk)))
    (%translate dx dy)))

(define (rotate degrees #! optional thunk)
  (if (default-object? thunk)
    (save-excursion (lambda _
      (%rotate degrees)
      (thunk)))
    (%translate dx dy)))

(define (forward dist #!optional thunk)
  (translate 0 dist thunk))

;;; Repeat a drawing task 'times.
;;; Each execution could change the transformation.
;;; Restore the transformation afterwards.
(define (repeat times thunk)
  (save-excursion (lambda _
    (let loop ((n times)))
      (if (> n 0)
        (begin
          (thunk)
          (transformer)
          (loop (- n 1)))))))

(define (mirror-x thunk)
  ...)

(define (square size)
  (repeat 4
    (lambda _
      (line 0 0 0 size)
      (forward 100)
      (rotate 90))))

