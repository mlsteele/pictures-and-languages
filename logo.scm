;;; Interpreter for Logo-like language.
;;;
;;; This is a miniature version of the Logo programming language.
;;; The basic premise of this language is that there is a turtle,
;;; a small robot who lives on a cartesian plane, and you tell the
;;; turtle how to draw spiffy things.
;;;
;;; The turtle knows several primitive commands:
;;; - how to move forwards (forward or fd)
;;; - how to turn to the right (rotate or rt)
;;; - how to pick up its pen to stop drawing (pen-up)
;;; - how to pick drop its pen to continue drawing (pen-down)
;;;
;;; The turtle starts at (0 0) with its pen down, ready to draw.
;;;
;;; You can teach the turtle tricks by defining procedures.
;;; Here we tell the turtle how to draw a square of a given size.
;;;     (to (square size)
;;;       (repeat 4
;;;         (fd size)
;;;         (rt 90))))
;;;
;;; And to invoke the square procedure and make the square happen:
;;;     (square 100)
;;;
;;; There are no return values from procedures in this Logo.
;;; In order to to interesting recursive things, use the
;;; limit form. The limit form takes a variable and a value
;;; and makes sure that the variable stays above the value.
;;; If the variable dips below the value, that call is immediately returned from.
;;; This can be used to make recursive base cases. For example:
;;;
;;; (to (spiral n)
;;;   (limit n 1)
;;;   (fd (/ n 20))
;;;   (rt 5)
;;;   (spiral (- n 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Turtle Type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A mutable logo turtle.
(define-record-type <logo:turtle>
    (%logo:turtle:new pos angle pendown)
    logo:turtle?
  (pos     logo:turtle:pos     logo:turtle:set-pos!)
  (angle   logo:turtle:angle   logo:turtle:set-angle!)
  (pendown logo:turtle:pendown logo:turtle:set-pendown!))

(define (logo:turtle:new)
  (%logo:turtle:new '(0 0) 0 #t))

(defhandler pp
  (lambda (turtle)
    (pp (list 'turtle
              (logo:turtle:pos turtle)
              (logo:turtle:angle turtle)
              (logo:turtle:pendown turtle))))
  logo:turtle?)

(define (logo:turtle:rotate turtle angle)
  (logo:turtle:set-angle! turtle
    (modulo (+ angle
               (logo:turtle:angle turtle))
            360)))

;;; Note: This does not draw any lines on any canvii
(define (logo:turtle:forward turtle distance)
  (let* ((pos (logo:turtle:pos turtle))
         (x (car pos))
         (y (cadr pos))
         (angle (logo:turtle:angle turtle))
         (rads (degrees->rads angle)))
     (logo:turtle:set-pos! turtle
       (list (+ x (* distance (cos rads)))
             (+ y (* distance (sin rads)))))))

#| Example
(define t (logo:turtle:new))
(pp t)
(logo:turtle:rotate t 90)
(pp t)
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Canvas Type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A Logo canvas holds the graphical state of a logo program.
;;; This includes where the turtle is, and what has been drawn so far.
(define-record-type <logo:canvas>
    (%logo:canvas:new turtle lines)
    logo:canvas?
  (turtle logo:canvas:turtle)
  (lines  logo:canvas:lines logo:canvas:set-lines!))

(define (logo:canvas:new)
  (%logo:canvas:new (logo:turtle:new) '()))

(define (logo:canvas:add-line canvas line)
  (logo:canvas:set-lines! canvas
    (cons line (logo:canvas:lines canvas))))

(define (logo:canvas->uniform canvas)
  (map (lambda (line)
         (let ((x1 (caar   line))
               (y1 (cadar  line))
               (x2 (caadr  line))
               (y2 (cadadr line)))
           (list 'line x1 y1 x2 y2)))
       (logo:canvas:lines canvas)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logo Language Recognizers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (logo:name? expr)
  (symbol? expr))

;;; A numexpr is anything which evaluates to a number.
;;; It could be a number, name, or numeric scheme expression
(define (logo:numexpr? expr)
  (or (logo:name? expr)
      (number? expr)
      (list? expr)))

(define ((match:->simple pattern) input)
  ((match:->combinators pattern)
   (list input)
   '()
   (lambda (d n) #t)))

(define logo:repeat?
  (match:->simple
    `(repeat (? count ,logo:numexpr?) (?? stmts))))

(define logo:to?
  (match:->simple
    `(to ((? name ,symbol?) (?? argnames)) (?? stmts))))

(define logo:limit?
  (match:->simple
    `(limit (? var ,symbol?) (? val ,logo:numexpr?))))

(define (logo:call? expr)
  (and (not (or (logo:to? expr)
                (logo:repeat? expr)
                (logo:limit? expr)))
       (list? expr)
       (not (null? expr))
       (logo:name? (car expr))
       (every logo:numexpr?
              (cdr expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logo Language Evaluators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Logo envs inherit things from the system
;;; environment so that arithmetic works.
(define (logo:make-env)
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
      ((pen-up)     (logo:builtin-pen 'up canvas))
      ((pen-down)   (logo:builtin-pen 'down canvas))
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
      (call-with-current-continuation (lambda (limit-stop)
        (for-each$ body (lambda (expr)
          (if (eq? (logo:eval expr invocation-env canvas) 'limit-reached)
            (limit-stop 'ok)))))))))

;;; repeat causes its body to be eval'd 'count times.
(define (logo:eval-repeat expr env canvas)
  (let* ((countexpr (cadr expr))
         (count (logo:eval-numexpr countexpr env canvas))
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

(define (logo:eval-limit expr env canvas)
  (let* ((valexpr (cadr expr))
         (limval  (caddr expr))
         (curval  (logo:eval-numexpr valexpr env varname)))
    (if (< curval limval)
      'limit-reached)))

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
(defhandler logo:eval (lookup-later 'logo:eval-limit)  logo:limit?)


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
    (if (logo:turtle:pendown turtle)
      (logo:canvas:add-line canvas
        (list oldpos (logo:turtle:pos turtle))))))

;;; Mode is 'up or 'down
(define (logo:builtin-pen mode canvas)
  (let ((turtle (logo:canvas:turtle canvas)))
    (case mode
      ((up)   (logo:turtle:set-pendown! turtle #f))
      ((down) (logo:turtle:set-pendown! turtle #t))
      (else (error 'invalid-pen-mode mode)))))


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
    (logo:eval-file "logo-library.scm" env canvas)
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

;;; Whether the repl should autoload logo-library
(define logo-load-lib #t)

(define (logo:eval-file filename env canvas)
  (call-with-input-file filename (lambda (port)
    (let loop ((input (read port)))
      (if (eof-object? input)
        'ok
        (begin
          (logo:eval input env canvas)
          (loop (read port))))))))

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
