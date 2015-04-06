
(define (ur:get-type ur)
  (assert (pair? ur))
  (assert (not (null? ur)))
  (car ur))

(define (ur:get-data ur)
  (assert (pair? ur))
  (cdr ur))

(define (ur:do-nothing)
  (ur:do-nothing-faster))
(define (ur:do-nothing-faster)
  (ur:do-nothing))
