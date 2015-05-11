;;; Here are some examples of execution for
;;; Context Free (CTXF)

(load "load")

#|
 (ctxf '(
	(startshape r)
	(shape random
	       (rule ((square (dr (random 90)))
		      (r (dr -2 y 0.1 s 0.9 0.9))))
	       (rule ((circle (s 0.9 0.9))
		      (r (dr 2  y 0.1 s 0.9 0.9)))))))

 (ctxf '(
	 (startshape foo (dr 45))
	 (shape foo (
		     (circle (s 0.01 0.01))
		     (triangle (y .5))
		     (bar (s 0.5 0.5))
		     ))
	 (shape bar (
		     (square (x 0.5))
		     ))
	 ))

 (ctxf '(
	 (startshape s)
	 (shape s (
		   (triangle (s 0.1 0.1))
		   (bar (dr 45))
		   ))
	 (shape bar (
		     (square (y 0.4))
		     (square (y 0.8))
		     ))
	 ))

 (ctxf '(
	 (startshape branch)
	 (shape branch
		(rule 3 (

			 (branch (dr 5 s 0.95 0.95))
			 (branch (dr -25 s 0.95 0.95))))
		(rule 99 (
			  (triangle (s 0.4 0.4))
			  (branch (dr 1 y 0.13 s 0.97 0.97))))
		)
	 ))


 (define (make-tree)
  (ctxf '(
	  (startshape branch)
	  (shape branch
		 (rule 5 (

			  (branch (dr 5 s 0.95 0.95))
			  (branch (dr -25 s 0.95 0.95))))
		 (rule 99 (
			   (fillTriangle (s 0.4 0.4))
			   (branch (dr 1 y 0.13 s 0.97 0.97))))
		 )
	  (shape fillTriangle (
			       (square ())
			       (fillTriangle (s 0.95 0.95))
			       ))
	  ))
)

 (define (make-tree-easier)
  (ctxf '(
	  (startshape branch)
	  (shape branch
		 (rule 7 (
			  (branch (dr 5 s 0.95 0.95))
			  (branch (dr -25 s 0.95 0.95))))
		 (rule 99 (
			   (fillTriangle (s 0.4 0.4))
			   (branch (dr 1 y 0.13 s 0.92 0.92))))
		 )
	  (shape fillTriangle (
			       (TRIANGLE ())
			       ))
	  ))
)

 (define (make-border)
  (ctxf '(
	  (startshape two-borders)
	  (shape two-borders (
			      (border-control (dr 180 y 4))
			      (border-control)
			      ))
	  (shape border-control (
				 (nearby-shapes (x (random 0.2)))
				 (border-control (x (random 1.0) s (+ 0.8 (random 0.2)) (+ 0.8 (random 0.2))))
				 ))
	  (shape nearby-shapes (
				(circle (y (+ -1.5 (random 1.5))))
				))
	  )))
				 
|#
