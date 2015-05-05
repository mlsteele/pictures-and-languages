;;;; CTXF Transform record deals with storing transforms
;;;; for our ctxf evaluator.

;; possible transforms:
;; x # translate by # in x
;; y # translate by # in y
;; {t, translate, trans} # # translate in x and y
;; {s, scale, size} # # scale in x and y, respectively
;; {dr, drotate, drot} # rotate ccw by # degrees
;; {rr, rrotate, rrot} # rotate ccw by # radians
;; {flipx, fx} flip across x axis
;; {flipy, fy} flip across y axis
;; {dflip, df} # flip across line through center that's # degrees above horiz
;; {rflip, rf} # flip across line through center that's # rads above horiz

;; A transform record contains both a stack of the transform commands up
;; to this point, as well as the net transformation matrix. We do this so
;; that we have access to the transform commands given if we ever need them
;; again. (This functionality is not implemented in our current code but
;; is available for extension.)
(define-record-type <transform>
  (%transform:new transforms-li matrix)
  transform?
  (matrix transform:matrix)
  (transforms-li transform:stack))

;; Given two transforms, combines them by appending
;; the transformations of t2 onto the end of the
;; transformations of t1. Does not mutate t1 or t2,
;; instead returns a new transform record.
(define (transform:combine t1 t2)
  (let ((new-transforms (append (transform:stack t1)
				(transform:stack t2)))
	(new-matrix (m:* (transform:matrix t2)
			 (transform:matrix t1))))
    (%transform:new new-transforms new-matrix)))

;; Given a transform record and a transformation params
;; list (as specified in ctxf), creates a new transform
;; record which is the original but with the operations
;; specified by the params list appended to those from
;; the original transform.
(define (transform:append transform t)
  (let ((transform-stack (append (transform:stack transform)
				 (list t)))
	(matrix (m:* (transform:matrix transform)
		     (transform:t->matrix t))))
    (%transform:new transform-stack matrix)))

;; Creates a new identity transform, i.e. a fresh transform
;; with nothing on the stack and an identity matrix
(define (transform:id)
  (%transform:new '() (m:identity)))

;; Given a transformation e.g. ( s 1 2 x 2 y 3 ... ), turns
;; it into a matrix in the natural basis.
(define (transform:t->matrix t)
  (define (t:->:do li)
    (if (null? li)
	(m:identity)
	(let ((m 
	       (case (car li)
		 ((x) (t:translate (cadr li) 0))
		 ((y) (t:translate 0 (cadr li)))
		 ((t translate trans) (t:translate (cadr li) (caddr li)))
		 ((s scale size) (t:scale (cadr li) (caddr li)))
		 ((dr drotate drot) (t:rotate 'degrees (cadr li)))
		 ((rr rrotate rrot) (t:rotate 'radians (cadr li)))
		 ((flipx fx) (t:flip 'degrees 0))
		 ((flipy fy) (t:flip 'degrees 90))
		 ((dflip df) (t:flip 'degrees (cadr li)))
		 ((rflip rf) (t:flip 'radians (cadr li)))))
	      (num-args
	       (case (car li)
		 ((flipx fx flipy fy) 0)
		 ((x y dr drotate drot rr rrotate rrot dflip df rflip rf) 1)
		 ((t translate trans s scale size) 2))))
	  (m:* (t:->:do (list-tail li (+ num-args 1))) m))))
  (t:->:do t))
