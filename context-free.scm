;;; Interpreter for Context Free (CTXF) language

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CTXF language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A CTXF canvas stores information about what shapes it has on it
(define-record-type <ctxf:canvas>
    (%ctxf:canvas:new shapes)
    ctxf:canvas?
  (lines ctxf:canvas:shapes ctxf:canvas:set-shapes!))

(define (ctxf:canvas:new)
  (%ctxf:canvas:new '()))

(define (ctxf:canvas:add-shape canvas shape)
  (ctxf:canvas:set-shapes! canvas
    (cons shape (ctxf:canvas:shapes canvas))))

(define (ctxf:canvas->uniform canvas)
  ;; todo
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
  (%ctxf:shape:new name body)
  ctxf:shape?
  (name     ctxf:shape:name)
  (body     ctxf:shape:body))

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
(define (ctxf:assign-var? expr)
  (or (tagged-list? expr 'let)
      (tagged-list? expr 'set)
      (tagged-list? expr 'set!)
      (tagged-list? expr '=)
      (tagged-list? expr 'define)
      (tagged-list? expr 'assign)))

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
		(ctxf:eval command env canvas))
	      input-lines)
    (ctxf:execute (car input-lines) env)))

(define ctxf:eval (make-generic-operator 3 'ctxf:eval))
(defhandler ctxf:eval (lookup-later 'ctxf:eval:startshape) ctxf:startshape?)
(defhandler ctxf:eval (lookup-later 'ctxf:eval:shape) ctxf:shape?)
(defhandler ctxf:eval (lookup-later 'ctxf:eval:primitive) ctxf:primitive?)
(defhandler ctxf:eval (lookup-later 'ctxf:eval:rule) ctxf:rule?)
(defhandler ctxf:eval (lookup-later 'ctxf:eval:shape-var) ctxf:shape-var?)
(defhandler ctxf:eval (lookup-later 'ctxf:eval:assign-var) ctxf:assign-var?)


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
  )

(define (ctxf:eval:assign-var expr env transform canvas)
  )

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


;;; Type for evaluated Logo procedures as defined to 'to statements.
;;; These are stored in env.
(define-record-type <logo:procedure>
    (logo:procedure:new name argnames body)
    logo:procedure?
  (name     logo:procedure:name)
  (argnames logo:procedure:argnames)
  (body     logo:procedure:body))

(define (logo:procedure-exists name env)
  (and (eq? (environment-reference-type env name) 'normal)
       (logo:procedure? (environment-lookup env name))))

(define (logo:eval-call expr env canvas)
  (let* ((name     (car expr))
         (argexprs (cdr expr))
         (argvals  (map (lambda (argexpr)
                          (logo:eval-numexpr argexpr env canvas))
                        argexprs)))
    (case name
      ((rotate rt)  (logo:builtin-rotate argvals canvas))
      ((forward fd) (logo:builtin-forward argvals canvas))
      (else
        (if (logo:procedure-exists name env)
          (logo:apply (environment-lookup env name)
                      argvals env canvas)
          (error 'call-unbound-logo-var name))))))

;;; Apply a logo:procedure called within 'env.
(define (logo:apply lproc argvals env canvas)
  (let ((name     (logo:procedure:name lproc))
        (argnames (logo:procedure:argnames lproc))
        (body     (logo:procedure:body lproc)))
    (if (not (= (length argnames) (length argvals)))
      (error 'arity-mismatch name))
    (let ((invocation-env (extend-top-level-environment
                            env argnames argvals)))
      (for-each (lambda (expr)
                  (logo:eval expr invocation-env canvas))
                body))))

;;; repeat causes its body to be eval'd 'count times.
(define (logo:eval-repeat expr env canvas)
  (let ((count (cadr expr))
        (stmts (cddr expr)))
    (do-n-times count
      (lambda _
        (for-each (lambda (stmt)
                    (logo:eval stmt env canvas))
                  stmts)))))

;;; Put a procedure definition in the environment.
(define (logo:eval-to expr env canvas)
  (let ((name     (caadr expr))
        (argnames (cdadr expr))
        (stmts    (cddr expr)))
    (environment-define env name
      (logo:procedure:new name argnames stmts))))

;;; This is not installed in the generic evaluator, because numexprs are
;;; not first-class citizens of the logo language.
;;; They are only the arguments to call's.
;;; We cheat here by using eval. This is a security vulnerability as arbitrary
;;; code with side-effects could occur here.
(define (logo:eval-numexpr expr env canvas)
  (eval expr env))

;;; Generic evaluator
(define logo:eval
  (make-generic-operator 3 'logo:eval))

(defhandler logo:eval (lookup-later 'logo:eval-repeat) logo:repeat?)
(defhandler logo:eval (lookup-later 'logo:eval-call)   logo:call?)
(defhandler logo:eval (lookup-later 'logo:eval-to)     logo:to?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logo Primitive Evaluators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These operations cause events to 'happen' by mutating the canvas.

(define (logo:builtin-rotate argvals canvas)
  (if (not (= 1 (length argvals)))
    (error 'arity-mismatch 'builtin-rotate))
  (let ((angle (car argvals))
        (turtle (logo:canvas:turtle canvas)))
    (logo:turtle:rotate turtle angle)))

(define (logo:builtin-forward argvals canvas)
  (if (not (= 1 (length argvals)))
    (error 'arity-mismatch 'builtin-forward))
  (let* ((distance (car argvals))
         (turtle (logo:canvas:turtle canvas))
         (oldpos (logo:turtle:pos turtle)))
    (logo:turtle:forward turtle distance)
    (logo:canvas:add-line canvas
      (list oldpos (logo:turtle:pos turtle)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Logo Programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define example-tosq
  '(to (square size)
     (repeat 4
       (fd size)
       (rt 90))))

(define example-callsq
  '(square 100))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| Example
(define c (logo:canvas:new))
(define e (logo:make-env))

(logo:eval '(rotate 10) e c)
(logo:eval '(fd 100) e c)
(logo:eval '(repeat 4 (fd 100)) e c)
(logo:eval '(repeat 4 (fd 100)) e c)
(logo:eval '(to (revline) (rt 180) (fd 100)) e c)
(logo:eval '(revline) e c)
(logo:eval '(to (square size) (repeat 4 (fd size) (rt 90))) e c)
(logo:eval '(square 50) e c)

(pp (logo:canvas:turtle c))
(pp c)
(pp (environment-bindings e))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logo REPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Start an interactive logo repl
;;; Terminate by entering "commit" or "(commit)"
;;; Returns the canvas
(define (logo:repl)
  (let ((canvas (logo:canvas:new))
        (env (logo:make-env)))
    (define (loop)
      (display "\nlogo> ")
      (let ((input (read)))
        (display "\n")
        (display input)
        (if (logo:repl-terminator? input)
          canvas
          (begin
            (logo:eval input env canvas)
            (loop)))))
    (loop)))

(define (logo:repl-terminator? input)
  (or (equal? input 'commit)
      (equal? input '(commit))))

#| Usage Example
(define result-canvas (logo:repl))
;;; You should now see a "logo>" prompt.

;;; Into REPL
(to (square size)
  (repeat 4
    (fd size)
    (rt 90)))
(square 100)
;;; (commit) terminates the repl and returns the canvas
(commit)

;;; Print the canvas, which contains 4 lines forming a square.
(pp result-canvas)
(pp (logo:canvas->uniform result-canvas))
|#
