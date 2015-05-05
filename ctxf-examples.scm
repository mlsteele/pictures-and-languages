;;; Here are some examples of execution for
;;; Context Free (CTXF)

#|
 (ctxf '(
	(startshape random)
	(shape random
	       (rule ((square ())
		      (random (dr -2 y 0.1 s 0.9 0.9))))
	       (rule ((circle ())
		      (random (dr 2  y 0.1 s 0.9 0.9)))))))
 
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
|#
