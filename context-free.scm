;;; Interpreter for Context Free (CTXF) language

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A CTXF canvas stores information about what shapes it has on it
(define-record-type <ctxf:canvas>
    (%ctxf:canvas:new shapes)
    ctxf:canvas?
  (shapes ctxf:canvas:shapes ctxf:canvas:set-shapes!))

(define (ctxf:canvas:new)
  (%ctxf:canvas:new '()))

(define (ctxf:canvas:add-shape canvas shape)
  (ctxf:canvas:set-shapes! canvas
    (cons shape (ctxf:canvas:shapes canvas))))

(define (ctxf:canvas->uniform canvas)
  3
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Definitions/Environment Sugar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ctxf:define var e env)
  (environment-define env var e))

(define (ctxf:make-env)
    (make-top-level-environment))

(define (ctxf:exists? name env)
  (eq? (environment-reference-type env name) 'normal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Shape/Variable Records
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <ctxf:shape>
  (%ctxf:shape:new name rules)
  ctxf:shape?
  (name     ctxf:shape:name)
  (rules     ctxf:shape:rules))

(define-record-type <ctxf:var>
  (%ctxf:var:new name val)
  ctxf:var?
  (name ctxf:var:name)
  (val  ctxf:var:val))

(define (ctxf:define-shape name body env)
  (ctxf:define name (%ctxf:shape:new name body) env))

(define (ctxf:define-var name val env)
  (ctxf:define name (%ctxf:var:new name val) env))

(define (ctxf:shape-exists? name env)
  (and (ctxf:exists? name env)
       (ctxf:shape? (environment-lookup env name))))

(define (ctxf:var-exists? name env)
  (and (ctxf:exists? name env)
       (ctxf:var? (environment-lookup env name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF Language Recognizers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ctxf:startshape? expr)
  (tagged-list? expr 'startshape))

(define (ctxf:shape? expr)
  (tagged-list? expr 'shape))

(define (ctxf:primitive? expr)
  (or (ctx:primitive:square? expr)
      (ctx:primitive:circle? expr) 
      (ctx:primitive:triangle? expr)))

(define (ctxf:primitive:square? expr)
  (tagged-list? expr 'square))

(define (ctxf:primitive:circle? expr)
  (tagged-list? expr 'circle))

(define (ctxf:primitive:triangle? expr)
  (tagged-list? expr 'triangle))

(define (ctxf:rule? expr)
  (tagged-list? expr 'rule))

;; x = ...
(define (ctxf:assign-const? expr)
  (or (tagged-list? expr 'let)
      (tagged-list? expr 'set)
      (tagged-list? expr 'set!)
      (tagged-list? expr '=)
      (tagged-list? expr 'define)
      (tagged-list? expr 'assign)
      (tagged-list? expr 'const)
      (tagged-list? expr 'constant)))


;; shapename [ ... ]
(define (ctxf:shape-var? expr env)
  (and (not (or (ctxf:startshape? expr)
		(ctxf:shape? expr)
		(ctxf:primitive? expr)
		(ctxf:rule? expr)
		(ctxf:assign-var? expr)))
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
	       (ctxf:startshape? (car input-lines))))
  (let ((env (ctxf:make-env)))
    (for-each (lambda (command)
		(ctxf:analyze command env canvas))
	      input-lines)
    (for-each (lambda (command)
		(ctxf:eval command env canvas))
	      input-lines)
    (ctxf:execute (car input-lines) env)))

(define ctxf:eval (make-generic-operator 3 'ctxf:eval))
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:startshape) ctxf:startshape?)
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:shape) ctxf:shape?)
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:primitive) ctxf:primitive?)
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:rule) ctxf:rule?)
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:shape-var) ctxf:shape-var?)
(defhandler ctxf:eval
  (lookup-later 'ctxf:eval:assign-const) ctxf:assign-const?)


(define (ctxf:numexpr? expr env)
  (or (number? expr)
      (list? expr)
      (ctxf:var-exists? expr env)))

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
(define (ctxf:transform-unify matrix t2)
  .. unify (i.e. typically multiply) transform matrices
  )


(define ctxf:analyze (make-generic-operator 2 'ctxf:analyze))
(defhandler ctxf:analyze
  (lookup-later 'ctxf:analyze:startshape) ctxf:startshape?)
(defhandler ctxf:analyze
  (lookup-later 'ctxf:analyze:shape) ctxf:shape?)
(defhandler ctxf:analyze
  (lookup-later 'ctxf:analyze:const) ctxf:assign-const?)

;;;;;;
;;;;;; analysis
;;;;;;
;;;;;;

;; only let them have one startshape
(define (ctxf:analyze:startshape expr env)
  (let ((name (cadr expr))
	(transforms (if (= 3 (length expr))
			(caddr expr)
			'())))
    (ensure (not (ctxf:exists? 'STARTSHAPE-EXISTS env))
	    "A startshape was already created!")
    (ctxf:define 'STARTSHAPE-EXISTS #t env)
    (ctxf:define 'STARTSHAPE-NAME name env)
    (ctxf:define-shape name transforms env)))

;; only let them assign a given shape once
(define (ctxf:analyze:shape expr env)
  (let ((name (cadr expr))
	(rules (if (> 2 (length epr))
		    (caddr expr)
		    '())))
    (if (ctxf:shape-exists? name env)
	(error "Shape already exists--cannot redefine it!" name))
    (ctxf:define-shape name (%ctxf:shape:new name rules))))

;; we'll only let them assign a constant once
(define (ctxf:analyze:const expr env)
  (let ((name (cadr expr))
	(val (caddr expr)))
    (ensure (not (ctxf:var-exists name env))
	    "Constants cannot be redefined!! Idiot")
    (ctxf:define-var name val env)))

(define (ctxf/test input-lines)
  (assert (and (not (null? input-lines))
	       (ctxf:startshape? (car input-lines))))
  (let ((env (ctxf:make-env)))
    (for-each (lambda (command)
		(ctxf:analyze command env canvas))
	      input-lines)))
#|
 (ctxf/test '(
	     (startshape X)
	     (shape X)
	     (shape Y)
	     (const x 3)))
|#
;;;;;;
;;;;;; eval
;;;;;;
;;;;;;


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

(define (ctxf:eval:assign-var expr env transform canvas)
  3)

;; ( ( ... ) ( ... ) ( ... ) )
(define (ctxf:eval-seq expr env transform canvas)
  (pp `(evaluating sequence ,expr)))

(define (rule:get-seq rule)
  (if (= (length rule) 3)
      (caddr rule)
      (cadr rule)))

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
	      

(define-record-type <ctxf:startshape>
  (%ctxf:startshape:new name params)
    ctxf:startshape/record?
  (name ctxf:startshape:name)
  (params ctxf:startshape:params))

(define-record-type <ctxf:startshape>
  (%ctxf:startshape:new name params)
    ctxf:startshape/record?
  (name ctxf:startshape:name)
  (params ctxf:startshape:params))
