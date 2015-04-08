;;;; Draw looks at the uniform representation and attempts to draw, using
;;;; Scheme's built-in graphics library, from an invariant representation.


(define (draw:point? expr)
  (and (tagged-list? expr 'point)
       (= (length expr) 3)))

(define (draw:line? expr)
  (and (tagged-list? expr 'line)
       (= (length expr) 5)))

(define device #f)
(define (draw:start-graphics!)
  (set! device (make-graphics-device (car (enumerate-graphics-types)))))

(define (draw:close-graphics!)
  (graphics-close device)
  (set! device #f))


(define (draw ur)
  (draw:start-graphics!)
  (for-each (lambda (ele) (draw:do device ele)) ur))

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

(define (draw:line:get ele expr)
  (assert (draw:line? expr))
  (cond ((eq? ele 'x1) (cadr expr))
	((eq? ele 'y1) (caddr expr))
	((eq? ele 'x2) (cadddr expr))
	((eq? ele 'y2) (fifth expr))
	(else
	 (error "Can't query ele from line" ele expr))))

(define (draw:slow-line d expr speed)
  (assert (draw:line? expr))
  (assert d)
  ;; todo... will fix soon.
  (let ((x1 (draw:line:get 'x1 expr))
	(y1 (draw:line:get 'y1 expr))
	(x2 (draw:line:get 'x2 expr))
	(y2 (draw:line:get 'y2 expr)))
    (graphics-draw-line d x1 y1 x2 y2)))

(define (draw:slow-point d expr speed)
  (draw:point d expr))
