;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pi (* 4 (atan 1.0)))

(define (degrees->rads degrees)
  (* 2 PI (/ degrees 360.)))

(define (true? x) (eq? #t x))

(define (truthy? x) (if x #t #f))

(define (do-n-times n proc)
  (let loop ((n n))
    (if (> n 0)
      (begin
        (proc)
        (loop (- n 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic pretty printer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pp
  (make-generic-operator 1 'pp
                         (environment-lookup system-global-environment 'pp)))

(defhandler pp
  (lambda _ (display "<null>"))
  null?)

#| Example
(pp '()) ; <null>
|#
