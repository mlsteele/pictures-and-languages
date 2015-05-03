;;; Used by everyone

(define (t:translate x y)
  (%matrix:new `((1 0 ,x)
		 (0 1 ,y)
		 (0 0 1))))

(define (t:scale sx sy)
  (%matrix:new `((,sx 0 0)
		 (0 ,sy 0)
		 (0 0 1))))

(define (t:rotate units val)
  (let ((theta (cond ((eq? units 'degrees)
		      (degrees->rads val))
		     ((eq? units 'radians)
		      radians)
		     (else
		      (error "Units of incorrect type!" units)))))
    (%matrix:new `((,(cos theta) ,(sin (- 0 theta)) 0)
		   (,(sin theta) ,(cos theta) 0)
		   (0 0 1)))))

(define (t:flip units val)
  (let* ((theta (cond ((eq? units 'degrees)
		       (degrees->rads val))
		      ((eq? units 'radians)
		       radians)
		      (else
		       (error "Units of incorrect type!" units))))
	 (lx (cos theta))
	 (ly (sin theta))
	 (a (- (* lx lx) (* ly ly))) ;; householder transform
	 (b (* 2 lx ly))
	 (c (* 2 lx ly))
	 (d (- (* ly ly) (* lx lx))))
    (%matrix:new `((,a ,b 0)
		   (,c ,d 0)
		   (0 0 1)))))
