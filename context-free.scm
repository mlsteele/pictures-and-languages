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
;;; Logo Language Recognizers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Recognize the three primitive shapes in CTXF
(define (ctxf:primitive:square? expr)
  (tagged-list? expr 'square))

(define (ctxf:primitive:circle? expr)
  (tagged-list? expr 'circle))

(define (ctxf:primtive-triangle? expr)
  (tagged-list? expr 'triangle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logo Language Evaluators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CTXF envs inherit things from the system
;;; environment so that arithmetic works.
(define (ctxf:make-env)
    (make-top-level-environment))

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
