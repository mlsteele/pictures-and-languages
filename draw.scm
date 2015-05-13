;;;; Draw looks at the uniform representation and attempts to draw, using
;;;; Scheme's built-in graphics library, from an invariant representation.
;;;; This is one of the backends we implemented.

;; Reuse device is for when you leave a graphics window open and want
;; to draw something in that window again, instead of closing it and
;; having the entity drawn on a new window.
(define *draw-reuse-device* #f)

(define (draw-reuse-device! #!optional reuse)
  (if (or (default-object? reuse)
           (eq? reuse #t))
    (set! *draw-reuse-device* #t)
    (set! *draw-reuse-device* #f))
  'ok)

;; Predicates for types of things to draw
(define (draw:point? expr)
  (and (tagged-list? expr 'point)
       (= (length expr) 3)))

(define (draw:line? expr)
  (and (tagged-list? expr 'line)
       (= (length expr) 5)))

(define (draw:color? expr)
  (and (tagged-list? expr 'color)
       (= (length expr) 2)))

;; Deal with the graphics device. Assumes you have at least one.
(define device #f)
(define (draw:start-graphics!)
  (set! device (make-graphics-device (car (enumerate-graphics-types)))))

(define (draw:close-graphics!)
  (and device
       (graphics-close device))
  (set! device #f))

;; Actually draws a UR
(define (draw ur)
  (if (or (eq? device #f)
          (not *draw-reuse-device*))
      (draw:start-graphics!))
  (let* ((width (graphics-device-width device))
         (height (graphics-device-height device))
         (ur (ur-fit-for-draw ur width height)))
    ;(graphics-operation device 'set-foreground-color "black")
    ;(graphics-operation device 'fill-circle 0 0 10)
    ;(graphics-operation device 'set-foreground-color "white")
    (for-each (lambda (ele)
      (draw:do device ele)) ur)))

(define test-ur '((line 0 0 1 1)
		  (line -1 -1 1/2 1/2)
		  (line -1/2 -1/2 1/2 -1/2)))
;;; Draw: operations assume a graphics device has been started.
(define (draw:do d expr)
  (assert d)
  (cond ((draw:point? expr)
	 (draw:point d expr))
	((draw:line? expr)
	 (draw:line d expr))
	((draw:color? expr)
	 (draw:color d expr))
	(else
	 (error "Not a valid uniform representation" expr))))

(define (draw:point d expr)
  (assert (draw:point? expr))
  (assert d)
  (let ((x (draw:point:get 'x expr))
	(y (draw:point:get 'y expr)))
    (graphics-draw-point d x y)))

(define (draw:line d expr)
  (assert (draw:line? expr))
  (assert d)
  (let ((x1 (draw:line:get 'x1 expr))
	(y1 (draw:line:get 'y1 expr))
	(x2 (draw:line:get 'x2 expr))
	(y2 (draw:line:get 'y2 expr)))
    (graphics-draw-line d x1 y1 x2 y2)))

(define (draw:point:get ele expr)
  (assert (draw:point? expr))
  (cond ((eq? ele 'x) (cadr expr))
	((eq? ele 'y) (caddr expr))
	(else
	 (error "Can't query ele from point" ele expr))))

(define (draw:color d expr)
  (assert (draw:color? expr))
  (assert d)
  (graphics-operation d 'set-foreground-color (cadr expr)))

(define (draw:line:get ele expr)
  (assert (draw:line? expr))
  (cond ((eq? ele 'x1) (cadr expr))
	((eq? ele 'y1) (caddr expr))
	((eq? ele 'x2) (cadddr expr))
	((eq? ele 'y2) (fifth expr))
	(else
	 (error "Can't query ele from line" ele expr))))

;; Can be improved in the future, but for now is just a way
;; to slowly draw a line. This screws you over if you have
;; tons of lines being drawn on the screen, but it's a 
;; good approximation for now.
(define (draw:slow-line d expr speed)
  (assert (draw:line? expr))
  (assert d)
  (define num-points 1000)
  (define ms-delay 1)
  (let ((x1 (draw:line:get 'x1 expr))
	(y1 (draw:line:get 'y1 expr))
	(x2 (draw:line:get 'x2 expr))
	(y2 (draw:line:get 'y2 expr)))
    (let ((pixels (get-line-pixels num-points x1 y1 x2 y2)))
      (for-each (lambda (px)
		  (graphics-draw-point d (car px) (cadr px))
		  (sleep ms-delay))
		pixels))))
(define (get-line-pixels num-points x1 y1 x2 y2)
  (define pixels (make-vector num-points))
  (define delx (/ (- x2 x1) (- num-points 1)))
  (define dely (/ (- y2 y1) (- num-points 1)))
  (define (get-pixels-do n)
    (if (= n num-points)
        pixels
	(begin
	  (vector-set! pixels n
		       (list (+ x1 (* n delx))
			     (+ y1 (* n dely))))
	  (get-pixels-do (+ n 1)))))
  (vector->list (get-pixels-do 0)))

;; Drawing a slow point is roughly meaningless
(define (draw:slow-point d expr speed)
  (draw:point d expr))
