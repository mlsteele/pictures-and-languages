;;; Interpreter for Context Free (CTXF) language

(load "load")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Canvas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A CTXF canvas stores information about all [primitive] shapes
;;;  on it.
(define-record-type <ctxf:canvas>
    (%ctxf:canvas:new shapes)
    ctxf:canvas?
  (shapes ctxf:canvas:shapes ctxf:canvas:shapes!))

(define (ctxf:canvas:new)
  (%ctxf:canvas:new '()))

(define (ctxf:canvas:add-shape canvas shape)
  (ctxf:canvas:shapes! canvas
    (append (ctxf:canvas:shapes canvas)
	    (list shape))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Definitions/Environment Sugar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ctxf:define var e env)
  (environment-define env var e))

(define (ctxf:lookup var env)
  (environment-lookup env var))

(define (ctxf:make-env)
    (make-top-level-environment))

(define (ctxf:exists? name env)
  (and (eq? (environment-reference-type env name) 'normal)
       (environment-bound? env name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Shapes & Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	      
(define-record-type <ctxf:startshape>
  (%ctxf:startshape:new name transforms)
    ctxf:startshape?
  (name ctxf:startshape:name)
  (transforms ctxf:startshape:transforms))

(define-record-type <ctxf:shape>
  (%ctxf:shape:new name rules)
  ctxf:shape?
  (name     ctxf:shape:name)
  (rules     ctxf:shape:rules))

(define-record-type <ctxf:const>
  (%ctxf:const:new name val)
  ctxf:const?
  (name ctxf:const:name)
  (val  ctxf:const:val))

(define (ctxf:define-shape name rules env)
  (ctxf:define name (%ctxf:shape:new name rules) env))

(define (ctxf:define-startshape name transforms env)
  (ctxf:define name (%ctxf:startshape:new name transforms) env))

(define (ctxf:define-const name val env)
  (ctxf:define name (%ctxf:const:new name val) env))

(define (ctxf:shape-exists? name env)
  (and (ctxf:exists? name env)
       (ctxf:shape? (ctxf:lookup name env))))

(define (ctxf:const-exists? name env)
  (and (ctxf:exists? name env)
       (ctxf:const? (ctxf:lookup env name))))

(define (ctxf:already-defined? name env)
  (and (ctxf:exists? name env)
       (or (ctxf:shape? (ctxf:lookup name env))
	   (ctxf:const? (ctxf:lookup name env)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Language Recognizers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ctxf:cmd/startshape? expr)
  (tagged-list? expr 'startshape))

(define (ctxf:cmd/shape? expr)
  (tagged-list? expr 'shape))

(define (ctxf:cmd/primitive? expr)
  (or (ctxf:cmd/primitive:square? expr)
      (ctxf:cmd/primitive:circle? expr) 
      (ctxf:cmd/primitive:triangle? expr)))

(define (ctxf:cmd/primitive:square? expr)
  (tagged-list? expr 'square))

(define (ctxf:cmd/primitive:circle? expr)
  (tagged-list? expr 'circle))

(define (ctxf:cmd/primitive:triangle? expr)
  (tagged-list? expr 'triangle))

(define (ctxf:cmd/rule? expr)
  (tagged-list? expr 'rule))

;; x = ...
(define (ctxf:cmd/assign-const? expr)
  (or (tagged-list? expr 'let)
      (tagged-list? expr 'set)
      (tagged-list? expr 'set!)
      (tagged-list? expr '=)
      (tagged-list? expr 'define)
      (tagged-list? expr 'assign)
      (tagged-list? expr 'const)
      (tagged-list? expr 'constant)))


;; shapename [ ... ]
(define (ctxf:cmd/shape-var? expr)
  (and (not (or (ctxf:cmd/startshape? expr)
		(ctxf:cmd/shape? expr)
		(ctxf:cmd/primitive? expr)
		(ctxf:cmd/rule? expr)
		(ctxf:cmd/assign-const? expr)))
       #t)) ; was (ctxf:shape-exists? (car expr) env)

;todo, will probably to matcher on (var = ...) somehow
;; also need to make sure that top level commands are only startshape, shape,
;; and rule. no primitives, no shape-var, no assign-var.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Language Evaluators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For now, the user will have to write everything inside a (ctxf '( ... ))
;;; function, e.g.
;;; (ctxf '(
;;;   (startshape S)
;;;   ...
;;;   ...))

;;; possible commands: startshape, shape, square, circle, triangle, rule,
;;;                    <shapename>, (let/set/set!/=/define/assign)

(define (ctxf input-lines)
  (assert (and (not (null? input-lines))
	       (ctxf:cmd/startshape? (car input-lines))))
  (let ((env (ctxf:make-env)))
    (for-each (lambda (command)
		(ctxf:analyze command env canvas))
	      input-lines)
    (for-each (lambda (command)
		(ctxf:eval command env canvas))
	      input-lines)
    (ctxf:execute (car input-lines) env)))

(define (ctxf:numexpr? expr env)
  (or (number? expr)
      (list? expr)
      (ctxf:const-exists? expr env)))

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


(define ctxf:analyze (make-generic-operator 2 'ctxf:analyze))
(defhandler ctxf:analyze
  (lookup-later 'ctxf:analyze:startshape) ctxf:cmd/startshape?)
(defhandler ctxf:analyze
  (lookup-later 'ctxf:analyze:shape) ctxf:cmd/shape?)
(defhandler ctxf:analyze
  (lookup-later 'ctxf:analyze:const) ctxf:cmd/assign-const?)

;;;;;;
;;;;;; analysis
;;;;;;
;;;;;;

;; only let them have one startshape
(define (ctxf:analyze:startshape expr env)
  (let ((name (symbol-append 'startshape// (cadr expr)))
	(transforms (if (= 3 (length expr))
			(caddr expr)
			'())))
    (ensure (not (ctxf:exists? 'STARTSHAPE-EXISTS env))
	    "Cannot have more than one startshape!")
    (ctxf:define 'STARTSHAPE-EXISTS #t env)
    (ctxf:define 'STARTSHAPE-NAME name env)
    (ctxf:define-startshape name transforms env)))

;; only let them assign a given shape once
(define (ctxf:analyze:shape expr env)
  (let ((name (cadr expr))
	(rules (if (> (length expr) 2)
		    (cddr expr)
		    '())))
    (if (ctxf:already-defined? name env)
	(error "An shape or constant with that name already
                 exists--cannot redefine it!" name))
    (ctxf:define-shape name rules env)))

;; we'll only let them assign a constant once
(define (ctxf:analyze:const expr env)
  (let ((name (cadr expr))
	(val (caddr expr)))
    (if (ctxf:already-defined? name env)
	(error "A shape or constant with that name already
               exists--cannot redefine it!"))
    (ctxf:define-const name val env)))

(define e #f)
(define (ctxf/test/analyze input-lines)
  (assert (and (not (null? input-lines))
	       (ctxf:cmd/startshape? (car input-lines))))
  (let ((env (ctxf:make-env)))
    (set! e env)
    (for-each (lambda (command)
		(ctxf:analyze command env))
	      input-lines)))
(define (lookup var) (environment-lookup e var))
(ctxf/test/analyze '(
	     (startshape X (p a r a m s))
	     (shape X (a s d f) (g h i j))
	     (shape Y (a b c d))
	     (shape z )
	     (const q 3)))

;;;;;;
;;;;;;
;;;;;; eval
;;;;;;
;;;;;;

(define ctxf:eval (make-generic-operator 4 'ctxf:eval))
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:startshape) ctxf:cmd/startshape?)
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:execute-shape) ctxf:cmd/shape-var?)
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:execute-primitive) ctxf:cmd/primitive?)
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:assign-const) ctxf:cmd/assign-const?)

(define (ctxf/test/eval input-lines)
  (assert (and (not (null? input-lines))
	       (ctxf:cmd/startshape? (car input-lines))))
  (let ((env (ctxf:make-env)))
    (set! e env)
    (for-each (lambda (command)
		(ctxf:analyze command env))
	      input-lines)
    (let ((identity-transform (transform:id))
	  (canvas (ctxf:canvas:new)))
      (set! c canvas)
      (ctxf:eval (car input-lines)
			    env
			    identity-transform
			    canvas))))

;; Checks if the transformation matrix is at the point where
;; shapes drawn can't really be seen by the human. We take
;; three test points, transform them, and if any of them are
;; too close together, we return true. The three test points
;; must be non-colinear. If they were colinear, it's possible
;; that the transformation would infinitely descend to 0 in
;; one dimension, yet we wouldn't catch it since we're looking
;; in the wrong dimension.
(define (too-small? transform)
  (define (dist p1 p2)
    (let* ((p1x (car p1))
	   (p1y (cadr p1))
	   (p2x (car p2))
	   (p2y (cadr p2))
	   (d (sqrt (+ (expt (- p1x p2x) 2)
		       (expt (- p1y p2y) 2)))))
      d))
  (let* ((matrix (transform:matrix transform))
	 (bl (list 0 -1))
	 (br (list 0 0))
	 (tr (list 1 0))
	 (tbl (m:*v matrix bl))
	 (tbr (m:*v matrix br))
	 (ttr (m:*v matrix tr))
	 (d1 (dist tbl tbr))
	 (d2 (dist tbr ttr))
	 (d3 (dist ttr tbl))
	 (threshold 1e-6))
    (or (< d1 threshold)
	(< d2 threshold)
	(< d3 threshold))))

(define (ctxf:eval:startshape expr env transform canvas)
  (let* ((name (cadr expr))
	 (tl (ctxf:startshape:transforms
	      (ctxf:lookup
	       (ctxf:lookup 'STARTSHAPE-NAME env)
	       env)))
	 (new-transform (transform:append transform tl)))
    (if (too-small? transform)
	'nothing
	(ctxf:draw name new-transform env canvas))))

(define (ctxf:draw shape-name transform env canvas)
  (if (too-small? transform)
      'stop-drawing
      (begin
	(if (memq shape-name '(SQUARE TRIANGLE CIRCLE))
	    (ctxf:draw:primitive shape-name transform canvas)
	    (begin
	      (ensure (ctxf:shape-exists? shape-name env)
		      "Can't draw a shape that doesn't exist!")
	      (let* ((shape-record (ctxf:lookup shape-name env))
		     (rules-list (ctxf:shape:rules shape-record))
		     (rule
		      (case (length rules-list)
			((0) 'do-nothing)
			((1) (ctxf:rule->content (car rules-list)))
			(else (ctxf:rule->content
			       (ctxf:rules->pick-one rules-list))))))
		(if (eq? rule 'do-nothing)
		    rule
		    (for-each$
		     rule
		     (lambda (expr) (ctxf:eval expr
					       env
					       transform
					       canvas))))))))))
(define (ctxf:do-nothing) (display "do-nothing\n") 3)
(define (ctxf:draw:primitive shape-name transform canvas)
  (pp `(draw-primitive ,shape-name :
	       ; ,(transform:stack transform) :
		,(matrix:vals (transform:matrix transform))))
  (ctxf:canvas:add-shape canvas
			     `(,shape-name
			       ,(transform:matrix transform))))

(define (ctxf:eval:execute-shape expr env transform canvas)
 ; (pp `(,expr / old-transform: ,(matrix:vals (transform:matrix transform))))
  (let* ((shape-name (car expr))
	 (t (cond ((= (length expr) 2)
		   (cadr expr))
		  ((= (length expr) 1)
		   '())
		  (else
		   (error "Call to execute shape, incorrect form!"))))
	 (new-transform (transform:append transform t)))
  ; (pp `(,expr / new-transform: ,(matrix:vals (transform:matrix new-transform))))
    (ctxf:draw shape-name new-transform env canvas)))

(define (ctxf:eval:execute-primitive expr env transform canvas)
  (ctxf:eval:execute-shape expr env transform canvas))

(define (ctxf:eval:assign-const expr env transform canvas)
  (ctxf:analyze:const expr env))

(define (ctxf:rule->content rule)
  (if (eq? (car rule) 'rule)
      (cond ((and (= (length rule) 2)
		  (pair? (cadr rule)))
	     (cadr rule))
	    ((and (= (length rule) 3)
		  (number? (cadr rule)))
	     (caddr rule))
	    (else
	     (error "Rule is not in the correct format!")))
      rule))

(define (ctxf:rules->pick-one rules)
  (let ((ps (ctxf:rule-probabilities rules))
	(r (random 1.0)))
    (let lp ((ind 0))
      (if (= ind (- (length ps) 1))
	  (last rules)
	  (if (< r (sum (list-head ps (+ ind 1))))
	      (list-ref rules ind)
	      (lp (+ ind 1)))))))

;; ( (rule ( ... )) (rule 0.3 ( ... )) )
(define (ctxf:rule-probabilities list-of-rules)
  (let* ((unweighted
	  (map$ list-of-rules
		(lambda (rule)
		  (if (number? (cadr rule))
		      (cadr rule)
		      1.0))))
	 (s (sum unweighted))
	 (weighted
	  (map$ unweighted
		(lambda (p)
		  (/ p s)))))
    weighted))


#|
 ;; (startshape x)
 ;; (startshape x (r 3 s 4 ... ))
 (define (ctxf:eval:startshape expr env transform canvas)
   (let* ((name (cadr expr))
	 (this-transform (if (= 3 (length expr)) (caddr expr) '()))
	 (new-transform (ctxf:transform-unify transform this-transform)))
     (ctxf:draw name new-transform)))

 ;; (shape x ( (...) (...) ... )
 ;; (shape x (rule ( ... )) (rule ( ... )) ... )
 (define (ctxf:eval:shape expr env transform canvas)
   (if (> (length expr) 3) ;; so we have rules
       (let ((ps (ctxf:rule-probabilities (cddr expr)))
	     (r (random 1.0)))
	 (let lp ((ind 0))
	   (if (= ind (- (length ps) 1))
	       (ctxf:eval-seq (rule:get-seq (last expr)))
	       (if (< r (sum (list-head ps (+ ind 1))))
		   (ctxf:eval-seq (rule:get-seq
				   (list-ref expr (+ ind 2)))) 
		   (lp (+ ind 1))))))
       (ctxf:eval-seq (caddr expr))))

 (define (ctxf:eval:primitive expr env transform canvas)
   (if (= (length expr) 1)
       (ctxf:draw-primitive (car expr) transform)
       (let* ((t (cadr expr))
	      (new-t (ctxf:transform-unify transform t)))
	 (ctxf:draw-primitive (car expr) new-t))))

 ;; (rule ( .... ) )
 (define (ctxf:eval:rule expr env transform canvas)
   (ctxf:eval-seq (rule:get-seq expr) env transform canvas))

 (define (ctxf:eval:shape-var expr env transform canvas)
   3)

 (define (ctxf:eval:assign-const expr env transform canvas)
   3)

 ;; ( ( ... ) ( ... ) ( ... ) )
 (define (ctxf:eval-seq expr env transform canvas)
   (pp `(evaluating sequence ,expr)))

 (define (rule:get-seq rule)
   (if (= (length rule) 3)
       (caddr rule)
       (cadr rule)))
|#


(define c #f)
(ctxf/test/eval '( (startshape x)
		   (shape x (
			     ;(foo (x 3 y 4))
			     ;(triangle ()) 
			     ;(square ())
			     (foo ())
			     ))
		   (shape foo (
			       (circle (x 0.1 y 0.1))
			       ))
		   ))

(ctxf/test/eval '( (startshape x)
		   (shape x (
			     (circle (s 0.01 0.01))
			     (triangle ())
			     (triangle (y 0.2))
			     (square ())
			     (foo (dr 90))
			     ))
		   (shape foo (
			       (triangle (y 0.2))
			       ))
		   ))


(ctxf/test/eval '( (startshape x)
		   (shape x (
			     (square ())
			     (x (s 0.9 0.9))
			     ))
		   (shape foo (
			       (triangle (y 0.2))
			       ))
		   ))

(ctxf/test/eval '( (startshape x)
		   (shape x (
			     (square ())
			     (x (y 1.01 s 0.99 0.99 dr -1))
			     ))
		   ))


(ctxf/test/eval '( (startshape x)
		   (shape x (
			     (circle (s 1.0 0.5))
			    ; (x (y 1.01 s 0.99 0.99 dr -1))
			     ))
		   ))












;;;;;;
;;;;
;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transforming Canvas Shapes to UR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list of e.g.: (square ((1 0 0) (0 1 0) (0 0 1)))
;; i.e. list of primitive shapes with their matrix transforms
(define (ctxf:canvas->uniform canvas)
  (define shapes (ctxf:canvas:shapes canvas))
  (define (canvas->uniform:do ind)
    (if (= ind (length shapes))
	'()
       	(append (canvas->uniform:do (+ ind 1))
		(ctxf:shape->uniform (list-ref shapes ind)))))
  (canvas->uniform:do 0))

(define ctxf:shape->uniform
  (make-generic-operator 1 'ctxf:shape->uniform))
(defhandler ctxf:shape->uniform
  (lookup-later 'ctxf:square->uniform) ctxf:cmd/primitive:square?)
(defhandler ctxf:shape->uniform
  (lookup-later 'ctxf:triangle->uniform) ctxf:cmd/primitive:triangle?)
(defhandler ctxf:shape->uniform
  (lookup-later 'ctxf:circle->uniform) ctxf:cmd/primitive:circle?)

(define (ctxf:square->uniform s)
  (let* ((coords (ctxf:square))
	 (matrix (cadr s))
	 (tl (m:*v matrix (car coords)))
	 (tr (m:*v matrix (cadr coords)))
	 (bl (m:*v matrix (caddr coords)))
	 (br (m:*v matrix (cadddr coords))))
    (list (list 'line (car tl) (cadr tl) (car tr) (cadr tr))
	  (list 'line (car tr) (cadr tr) (car br) (cadr br))
	  (list 'line (car br) (cadr br) (car bl) (cadr bl))
	  (list 'line (car bl) (cadr bl) (car tl) (cadr tl)))))

(define (ctxf:triangle->uniform s)
  (let* ((coords (ctxf:triangle))
	 (matrix (cadr s))
	 (tp (m:*v matrix (car coords)))
	 (br (m:*v matrix (cadr coords)))
	 (bl (m:*v matrix (caddr coords))))
    (list (list 'line (car tp) (cadr tp) (car br) (cadr br))
	  (list 'line (car br) (cadr br) (car bl) (cadr bl))
	  (list 'line (car bl) (cadr bl) (car tp) (cadr tp)))))

(define (ctxf:circle->uniform s)
  (let* ((r (ctxf:circle))
	 (matrix (cadr s))
	 (num-points 1000)
	 (points (make-vector num-points)))
    (let lp ((ind 0)
	     (theta 0))
      (if (= ind num-points)
	  (vector->list points)
	  (let* ((xy (list (* (cos theta) r) (* (sin theta) r)))
		 (txy (m:*v matrix xy)))
	    (vector-set! points ind
			 (list 'point (car txy) (cadr txy)))
	    (lp (+ ind 1) (* 2 pi (+ ind 1) (/ 1.0 num-points))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primitive Shapes Key Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cw starting top left, coords
(define (ctxf:square)
  (list (list (/ -1 2) (/ 1 2))
	(list (/ 1 2) (/ 1 2))
	(list (/ -1 2) (/ -1 2))
	(list(/ 1 2) (/ -1 2))))

;; cw starting top, coords
(define (ctxf:triangle)
  (list (list 0 (/ 1 (sqrt 3)))
	(list (/ 1 2) (/ (sqrt 3) -6))
	(list (/ -1 2) (/ (sqrt 3) -6))))

;; radius of circle
(define (ctxf:circle)
  (/ 1 2))

(draw (ctxf:canvas->uniform c))
