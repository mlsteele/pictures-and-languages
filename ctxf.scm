;;; Interpreter for Context Free (CTXF) language
;;;
;;; This is the interpreter for a modified version of the Context
;;; Free Art language. If you desire more information, please visit
;;; their website: http://www.contextfreeart.org
;;;
;;; Possible commands: startshape, shape, <shape-name>, <primitive>,
;;;                    {let/set/set!/=/define/assign/const/constant}
;;; Where <shape-name> was defined with a (shape <shape-name> ...) command,
;;; and <primitive> is one of {triangle, square, circle}.
;;;
;;; All programs must start with a (startshape <s>) command, which declares
;;; the startshape to be <s>. Actually, the startshape command can be of the form:
;;;  (startshape <s>), (startshape <s> ()), or (startshape <s> (args...))
;;; where the first two are equivalent and the third one invokes transformation
;;; parameters on the startshape, as explained later. 
;;; There must be exactly one startshape command in the program.
;;;
;;; The user must then call a (shape <s> ... ) command to declare rules for
;;; the shape. These rules explain what is done/drawn when the shape is
;;; invoked. There can only be one shape command per shape-name. The shape
;;; command can be one of two forms:
;;;  (shape <s> <command-list>) or
;;;  (shape <s> (rule <?num> <command-list>) ... (rule <?num> <command-list>))
;;; where <?num> represents an optional numeric argument, and <command-list>
;;; is a list of commands, of the form
;;;  (<command-1> ... <command-n>)
;;; where each <command-i> is a command like (square ... ), (<shape-name> ...),
;;; etc.
;;; In the second case, the evaluator picks one of the rules randomly, based
;;; on the weights supplied by the user. The optional argument <?num> is the
;;; weight assigned, and if no weight is assigned, assume it's 1. The weights
;;; are normalized over 1, and a rule is picked based on the weights.
;;;
;;; Regardless, once we have a <command-list> to evaluate, we go through 
;;; one command at a time, evaluating each one in turn. Invocations to
;;; draw primitives are executed and drawn immediately, while invocations
;;; to draw non-primitives are evaluated recursively.
;;;
;;; To make an invocation to draw a primitive shape, we call
;;; (<primitive-name>) or (<primitive-name> ()) or (<primitive-name> (args...))
;;; The first two are equivalent, and call to draw the primitive shape
;;; with no modifications. The third requests to draw the primitive shape
;;; with the transformation parameters.
;;;
;;; Similarly, we invoke a drawing on a non-primitive by calling
;;; (<shape-name>) or (<shape-name> ()) or (<shape-name> (args...))
;;; which attempts to draw that shape according to rules declared by the user.
;;;
;;; A user can assign a constant by calling, for example,
;;;  (let <const-name> <expr>)
;;; where <expr> can either be a number or an expression that evaluates to a
;;; number. The expression can itself refer to other constants previously
;;; defined.
;;;
;;; Transformation parameters can be of the following form:
;;;  x # translate by # in x
;;;  y # translate by # in y
;;;  {t, translate, trans} # # translate in x and y
;;;  {s, scale, size} # # scale in x and y, respectively
;;;  {dr, drotate, drot} # rotate ccw by # degrees
;;;  {rr, rrotate, rrot} # rotate ccw by # radians
;;;  {flipx, fx} flip across x axis
;;;  {flipy, fy} flip across y axis
;;;  {dflip, df} # flip across line through center that's # degrees above horiz
;;;  {rflip, rf} # flip across line through center that's # rads above horiz
;;;
;;; To apply some transformations to an invocation (for example, on foo),
;;; we call (foo (args...)). (args...) is the sequence of requested 
;;; transformations, in left-to-right order, e.g.
;;;  (x 3 s 0.5 0.5 rr 10)
;;; this example translates 3 to the right, scales by 0.5 in both dimensions,
;;; and then rotates ccw by 10 radians.
;;; Transformations are applied in the current frame of reference. So, if a
;;; call to foo is made with a 45-degree rotation transformation request,
;;; then any invocation made within foo that requests a transformation is
;;; applied in the current rotated frame, so, for example, a translation by
;;; 1 in x is actually a translation to the upper-right by 1 in the original
;;; frame of reference. Hence, as we progress down the call chain
;;; recursively, we compound transformations.


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
;;; CTXF Environment Sugar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ctxf:define var e env)
  (environment-define env var e))

(define (ctxf:lookup var env)
  (environment-lookup env var))

(define (ctxf:make-env)
    (make-top-level-environment))

;; Checks if the variable is bound in the environment
(define (ctxf:exists? name env)
  (and (eq? (environment-reference-type env name) 'normal)
       (environment-bound? env name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Shapes & Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We store the analyzed definitions of startshapes, shapes,
;;; and constants in records so we can access data about them
;;; while evaluating and drawing.

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
       (ctxf:const? (ctxf:lookup name env))))

;; Checks if a shape or a const has been declared already. 
;; Does not check if the startshape shares the name.
(define (ctxf:already-defined? name env)
  (and (ctxf:exists? name env)
       (or (ctxf:shape? (ctxf:lookup name env))
	   (ctxf:const? (ctxf:lookup name env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Language Recognizers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Predicates to decide what type of command a command
;;; is, when analyzing and evaluating.

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

;; We allow users to use a variety of commands to
;; assign a const
(define (ctxf:cmd/assign-const? expr)
  (or (tagged-list? expr 'let)
      (tagged-list? expr 'set)
      (tagged-list? expr 'set!)
      (tagged-list? expr '=)
      (tagged-list? expr 'define)
      (tagged-list? expr 'assign)
      (tagged-list? expr 'const)
      (tagged-list? expr 'constant)))

;; Assumes any command that's not of some other
;; form is a call to execute a shape
(define (ctxf:cmd/shape-var? expr)
  (and (not (or (ctxf:cmd/startshape? expr)
		(ctxf:cmd/shape? expr)
		(ctxf:cmd/primitive? expr)
		(ctxf:cmd/rule? expr)
		(ctxf:cmd/assign-const? expr)))
       #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Language Analyzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; At the beginning of execution, the program is first
;;; analyzed to store high-level definitions of startshapes,
;;; shapes, and consts
(define ctxf:analyze (make-generic-operator 2 'ctxf:analyze))
(defhandler ctxf:analyze
  (lookup-later 'ctxf:analyze:startshape) ctxf:cmd/startshape?)
(defhandler ctxf:analyze
  (lookup-later 'ctxf:analyze:shape) ctxf:cmd/shape?)
(defhandler ctxf:analyze
  (lookup-later 'ctxf:analyze:const) ctxf:cmd/assign-const?)

;; By the way, we only let them have one startshape
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

;; You can only assign a shape definition once
(define (ctxf:analyze:shape expr env)
  (let ((name (cadr expr))
	(rules (if (> (length expr) 2)
		    (cddr expr)
		    '())))
    (if (ctxf:already-defined? name env)
	(error "An shape or constant with that name already
                 exists--cannot redefine it!" name))
    (ctxf:define-shape name rules env)))

;; You can only assign a constant once
(define (ctxf:analyze:const expr env)
  (let ((name (cadr expr))
	(val (caddr expr)))
    (if (ctxf:already-defined? name env)
	(error "A shape or constant with that name already
               exists--cannot redefine it!"))
    (ctxf:define-const name val env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Language Evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; After analysis, we attempt to execute the startshape,
;;; which will in turn force us to recursively evaluate
;;; the commands in the definition of the startshape, and
;;; so on.

(define ctxf:eval (make-generic-operator 4 'ctxf:eval))
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:startshape) ctxf:cmd/startshape?)
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:execute-shape) ctxf:cmd/shape-var?)
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:execute-primitive) ctxf:cmd/primitive?)
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:assign-const) ctxf:cmd/assign-const?)

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
	 (threshold 1e-2))
    (or (< d1 threshold)
	(< d2 threshold)
	(< d3 threshold))))

(define (ctxf:eval:startshape expr env transform canvas)
  (let* ((name (cadr expr))
	 (tl (ctxf:startshape:transforms
	      (ctxf:lookup
	       (ctxf:lookup 'STARTSHAPE-NAME env)
	       env)))
	 (new-transform (transform:append transform
					  (ctxf:t->resolve-consts tl env))))
    (if (too-small? transform)
	'nothing
	(ctxf:draw name new-transform env canvas))))

(define (ctxf:eval:execute-shape expr env transform canvas)
  (let* ((shape-name (car expr))
	 (t (cond ((= (length expr) 2)
		   (cadr expr))
		  ((= (length expr) 1)
		   '())
		  (else
		   (error "Call to execute shape, incorrect form!"))))
	 (new-transform (transform:append transform
					  (ctxf:t->resolve-consts t env))))
    (ctxf:draw shape-name new-transform env canvas)))

(define (ctxf:eval:execute-primitive expr env transform canvas)
  (ctxf:eval:execute-shape expr env transform canvas))

(define (ctxf:eval:assign-const expr env transform canvas)
  (ctxf:analyze:const expr env))

;;; Takes a transformation list and resolves all of the constants
;;; referenced within, if any.
(define (ctxf:t->resolve-consts t env)
  (define t-evald (list-copy t))
  (let lp ((ind 0))
    (if (>= ind (length t))
	t-evald
	(let ((num-args
	      (case (list-ref t ind)
		((flipx fx flipy fy) 0)
		((x y dr drotate drot rr rrotate rrot dflip df rflip rf) 1)
		((t translate trans s scale size) 2))))
	  (let lp-eval ((j (+ ind 1)))
	    (if (> j (+ ind num-args))
		(lp (+ ind num-args 1))
		(begin
		  (list-set!
		   t-evald
		   j
		   (ctxf:const->resolve (list-ref t j) env))
		  (lp-eval (+ j 1)))))))))

;; Fully resolves a constant defined in the environment
(define (ctxf:const->resolve const env)
  (define (eval/c val env)
    (if (pair? val)
	(let* ((op (eval (car val) env))
	       (args (cdr val))
	       (list-mapped-evald-args (map (lambda (ele)
					      (eval/c ele env))
					    args)))
	  (apply op list-mapped-evald-args))
	(let ((evald (eval val env)))
	  (if (ctxf:const? evald)
	      (eval/c (ctxf:const:val evald) env)
	      evald))))
  (eval/c const env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Drawing to Canvas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; During evaluation, we attempt to draw any of the
;;; shapes that are invoked. This will draw either a
;;; primitive or a non-primitive (in which case it
;;; calls to evaluate the commands within).

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

;; Draws a primitive object to the canvas record.
(define (ctxf:draw:primitive shape-name transform canvas)
  (ctxf:canvas:add-shape canvas
			     `(,shape-name
			       ,(transform:matrix transform))))

;;; Takes a rule and extracts its content. Can take any of:
;;; (rule ( ... ))
;;; (rule <num> ( ... ))
;;; ( ... )
;;; the result will be the content, i.e. ( ... )
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

;;; Given a list of rules with possibly-supplied probabilities,
;;; picks one based on the probabilities.
(define (ctxf:rules->pick-one rules)
  (let ((ps (ctxf:rule-probabilities rules))
	(r (random 1.0)))
    (let lp ((ind 0))
      (if (= ind (- (length ps) 1))
	  (last rules)
	  (if (< r (sum (list-head ps (+ ind 1))))
	      (list-ref rules ind)
	      (lp (+ ind 1)))))))

;;; Given a list of rules with possibly-supplied probabilities,
;;; returns a list of weighted probabilities corresponding to
;;; each of the rules.
;;; Example: calling this method on
;;; ((rule (...)) (rule 0.5 (...)) (rule 1.5 (...)))
;;; would yield (0.3333 0.16666 0.5)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Transforming Canvas Shapes to UR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; At the end of the day, we need to transform the shapes
;;; listed in the canvas record to the uniform representation.
;;; Becuase all of the transformations we do to shapes are
;;; quasi-linear, line-straightness is preserved: hence, a
;;; square or triangle can be successfully represented by
;;; its four (or three) constituent lines which can be stored
;;; in the UR as lines whose constituent points are transformed
;;; by the transformation matrix.
;;; The shapes list in the canvas record is a list of primitive
;;; formulations, each of the form
;;;  (<primitive-shape> <matrix-list-representation>)
;;; e.g.
;;;  (square ((1 0 0) (0 1 0) (0 0 1)))
;;; which, in this case, signifies a Context Free unit
;;; square that's transformed by the matrix.

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
;;; Primitive Shapes: Key Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Coordinates of the vertices, starting at top
;; left and going clockwise
(define (ctxf:square)
  (list (list (/ -1 2) (/ 1 2))
	(list (/ 1 2) (/ 1 2))
	(list (/ -1 2) (/ -1 2))
	(list(/ 1 2) (/ -1 2))))

;; Coordinates of the vertices, starting at the
;; top and going clockwise
(define (ctxf:triangle)
  (list (list 0 (/ 1 (sqrt 3)))
	(list (/ 1 2) (/ (sqrt 3) -6))
	(list (/ -1 2) (/ (sqrt 3) -6))))

;; Radius of circle
(define (ctxf:circle)
  (/ 1 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is what the user calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This function takes a list of commands and evaluates the program,
;; thereafter drawing the shapes to the screen with X. The method
;; will be called like this:
;; (ctxf '(
;;   (startshape S)
;;   ...
;;   ...))
(define (ctxf input-lines)
  (assert (and (not (null? input-lines))
	       (ctxf:cmd/startshape? (car input-lines))))
  (display "Evaluating Context Free drawing...\n")
  (let ((env (ctxf:make-env)))
    (for-each (lambda (command)
		(ctxf:analyze command env))
	      input-lines)
    (let ((identity-transform (transform:id))
	  (canvas (ctxf:canvas:new)))
      (ctxf:eval (car input-lines)
			    env
			    identity-transform
			    canvas)
      (draw (ctxf:canvas->uniform canvas))
      'done)))
