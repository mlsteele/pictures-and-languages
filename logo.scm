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
;;; - how to pick up its pen to stop drawing (penup)
;;; - how to pick drop its pen to continue drawing (pendown)
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
           (list 'point x1 y1 x2 y2)))
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

(define (logo:call? expr)
  (and (list? expr)
       (not (or (null? expr)
                (logo:to? expr)
                (logo:repeat? expr)))
       (every logo:numexpr?
              (cdr expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logo Language Evaluators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define logo:eval
  (make-generic-operator 3 'logo:eval
    (lambda args (apply logo:eval-default args))))

(define (logo:eval-default expr env canvas)
  (case (car expr)
    ((rotate rt) (logo:builtin-rotate expr env canvas))
    ((forward fd) (logo:builtin-forward expr env canvas))
    (else (error 'call-not-implemented expr))))

;;; repeat causes its body to be eval'd 'count times.
(defhandler logo:eval
  (lambda (expr env canvas)
    (let ((count (cadr expr))
          (stmts (cddr expr)))
      (do-n-times count
        (lambda _
          (for-each (lambda (stmt)
                      (logo:eval stmt env canvas))
                    stmts)))))
  logo:repeat?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logo Primitive Evaluators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These operations cause events 'happen' by mutating the canvas.

(define (logo:builtin-rotate expr env canvas)
  (let ((turtle (logo:canvas:turtle canvas))
        (angle (cadr expr)))
    (logo:turtle:rotate turtle angle)))

(define (logo:builtin-forward expr env canvas)
  (let* ((distance (cadr expr))
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

(logo:eval '(rotate 10) '() c)
(logo:eval '(fd 100) '() c)
(logo:eval '(repeat 4 (fd 100)) '() c)

(pp (logo:canvas:turtle c))
(pp c)
|#
