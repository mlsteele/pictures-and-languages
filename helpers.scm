;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pi (* 4 (atan 1.0)))

(define (degrees->rads degrees)
  (* 2 pi (/ degrees 360)))

(define (rads->degrees rads)
  (* rads (/ 360 (* 2 pi))))

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

(define (sleep millis)
  (sleep-current-thread (floor->exact millis)))

(define (sleep-seconds secs)
  (sleep-current-thread (floor->exact (* 1000 secs))))

(define (sum li)
  (fold-left + 0 li))

(define (reset) (ge (make-top-level-environment)))

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

;;; Apply each of procs to each corresponding value.
(define (zip-apply procs values)
  (ensure (= (length procs) (length values))
          "zip-apply procs and values must have same length")
  (if (null? values)
    '()
    (cons ((car procs) (car values))
          (zip-apply (cdr procs) (cdr values)))))

#| Test Cases
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(zip-apply (list inc inc dec) '(5 12 40)) ; '(6 13 39)
|#

(define (for-each$ list f) (map f list))
(define (map$ list f) (map f list))

(define (line-length x1 y1 x2 y2)
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (cc (+ (* dx dx) (* dy dy)))
         (c (sqrt cc)))
    c))

(define (graphics-device-width d)
  ((graphics-device-coordinate-limits d)
   (lambda (x-left y-bottom x-right y-top)
     (abs (- x-right x-left)))))

(define (graphics-device-height d)
  ((graphics-device-coordinate-limits d)
   (lambda (x-left y-bottom x-right y-top)
     (abs (- y-top y-bottom)))))



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
