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

(define (tagged-list? expr tag)
  (if (pair? expr)
      (eq? (car expr) tag)
      #f))

;;; Lookup a procedure when it is called.
;;; This defends against pass-by-value binding of procedures.
;;; This makes development easier, but a little more dangerous.
(define (((lookup-later-in env) name) . args)
  (apply (environment-lookup env name)
         args))

(define lookup-later (lookup-later-in (the-environment)))

#| Test Cases
(define (foo) 1)
(define foo-proxy (lookup-later 'foo))
(foo-proxy) ; 1
(define (foo) 2)
(foo-proxy) ; 2
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic pretty printer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pp
  (make-generic-operator 1 'pp
                         (environment-lookup system-global-environment 'pp)))

(defhandler pp
  (lambda _ (display "<null>\n"))
  null?)

#| Example
(pp '()) ; <null>
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Safety Checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Error reporting helper
(define (ensure predicate message)
  (let ((nil '()))
    (if (not (eq? #t predicate))
      (error message))))


#| Test Cases
(ensure #t "bad news") ; nothing
(ensure #f "bad news") ; "bad news"
(ensure 1 "bad news") ; "bad news"
(ensure (lambda () #t) "bad news") ; "bad news" (b/c lambda is not true)
|#
