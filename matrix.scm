;;;; 2x2 Matrix Class

(define-record-type <matrix>
  (%matrix:new a b c d)
  matrix?
  (a matrix:a matrix:a!)
  (b matrix:b matrix:b!)
  (c matrix:c matrix:c!)
  (d matrix:d matrix:d!))

(define (matrix:vals m)
  (list (matrix:a m)
	(matrix:b m)
	(matrix:c m)
	(matrix:d m)))

(define (m:* m1 m2)
  (let ((a (+ (* (matrix:a m1) (matrix:a m2))
	      (* (matrix:b m1) (matrix:c m2))))
	(b (+ (* (matrix:a m1) (matrix:b m2))
	      (* (matrix:b m1) (matrix:d m2))))
	(c (+ (* (matrix:c m1) (matrix:a m2))
	      (* (matrix:d m1) (matrix:c m2))))
	(d (+ (* (matrix:c m1) (matrix:b m2))
	      (* (matrix:d m1) (matrix:d m2)))))
    (%matrix:new a b c d)))

(define (m:c* c m)
  (let ((c-mat (%matrix:new c 0 0 c)))
    (m:* c-mat m)))

(define (m:*c m c)
  (m:c* c m))

(define (m:/ m c)
  (m:c* (/ 1 c) m))

(define (m:negate m)
  (m:c* -1 m))

(define (m:+ m1 m2)
  (let ((a (+ (matrix:a m1) (matrix:a m2)))
	(b (+ (matrix:b m1) (matrix:b m2)))
	(c (+ (matrix:c m1) (matrix:c m2)))
	(d (+ (matrix:d m1) (matrix:d m2))))
    (%matrix:new a b c d)))

(define (m:- m1 m2)
  (m:+ m1 (m:negate m2)))

(define (m:print m)
  (pp `(matrix ,(matrix:a m) ,(matrix:b m)
	       ,(matrix:c m) ,(matrix:d m))))
