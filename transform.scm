(define-record-type <transform>
  (%transform:new transforms-li matrix)
  transform?
  (matrix transform:matrix)
  (transforms-li transform:stack))

(define (transform:combine t1 t2)
  (let ((new-transforms (append (transform:stack t1)
				(transform:stack t2)))
	(new-matrix (m:* (transform:matrix t2)
			 (transform:matrix t1))))
    (%transform:new new-transforms new-matrix)))

;; like
;; (transform:append <transform-object> (r 1 s 2 ...))
(define (transform:append t transformation)
  (let ((transforms (append (transform:stack t)
			    (list transformation)))
	(matrix (t:->matrix transformation)))
    (%transform:new transforms matrix)))

;; Given a transformation e.g. ( s 1 x 2 y 3 ... ), turns
;; it into a matrix in the natural basis
(define (t:->matrix t)
  (define m (%matrix:new '((1 0 0) (0 1 0) (0 0 1))))
  (define (t:->:do li)
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
      (m:* (t:->:do (list-tail li (+ num-args 1))) m)))
  (t:->:do t))


