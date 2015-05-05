;;;; This file deals with 3x3 matrices. We use 3x3 matrices
;;;; to do 2-dimensional transformations (including
;;;; translations, via homogeneous coordinates). We use
;;;; a record to store the matrix data.

(define-record-type <matrix>
  (%matrix:new list-of-lists)
  matrix?
  (list-of-lists matrix:vals matrix:vals!))

(define (matrix:ind ind m)
  (let ((li (matrix:vals m))
	(c (modulo ind 3))
	(r (floor (/ ind 3))))
    (matrix:rc r c m)))

(define (matrix:rc r c m)
  (list-ref (list-ref (matrix:vals m) r) c))

(define (m:identity)
  (%matrix:new '((1 0 0) (0 1 0) (0 0 1))))

(define (m:* m1 m2)
  (let ((a (+ (* (matrix:ind 0 m1) (matrix:ind 0 m2))
	      (* (matrix:ind 1 m1) (matrix:ind 3 m2))
	      (* (matrix:ind 2 m1) (matrix:ind 6 m2))))
	(b (+ (* (matrix:ind 0 m1) (matrix:ind 1 m2))
	      (* (matrix:ind 1 m1) (matrix:ind 4 m2))
	      (* (matrix:ind 2 m1) (matrix:ind 7 m2))))
	(c (+ (* (matrix:ind 0 m1) (matrix:ind 2 m2))
	      (* (matrix:ind 1 m1) (matrix:ind 5 m2))
	      (* (matrix:ind 2 m1) (matrix:ind 8 m2))))
	(d (+ (* (matrix:ind 3 m1) (matrix:ind 0 m2))
	      (* (matrix:ind 4 m1) (matrix:ind 3 m2))
	      (* (matrix:ind 5 m1) (matrix:ind 6 m2))))
	(e (+ (* (matrix:ind 3 m1) (matrix:ind 1 m2))
	      (* (matrix:ind 4 m1) (matrix:ind 4 m2))
	      (* (matrix:ind 5 m1) (matrix:ind 7 m2))))
	(f (+ (* (matrix:ind 3 m1) (matrix:ind 2 m2))
	      (* (matrix:ind 4 m1) (matrix:ind 5 m2))
	      (* (matrix:ind 5 m1) (matrix:ind 8 m2))))
	(g (+ (* (matrix:ind 6 m1) (matrix:ind 0 m2))
	      (* (matrix:ind 7 m1) (matrix:ind 3 m2))
	      (* (matrix:ind 8 m1) (matrix:ind 6 m2))))
	(h (+ (* (matrix:ind 6 m1) (matrix:ind 1 m2))
	      (* (matrix:ind 7 m1) (matrix:ind 4 m2))
	      (* (matrix:ind 8 m1) (matrix:ind 7 m2))))
	(i (+ (* (matrix:ind 6 m1) (matrix:ind 2 m2))
	      (* (matrix:ind 7 m1) (matrix:ind 5 m2))
	      (* (matrix:ind 8 m1) (matrix:ind 8 m2)))))
    (%matrix:new (list (list a b c)
		       (list d e f)
		       (list g h i)))))

;; assume that vector is a list of the form (x y) or (x y w)
;; (the latter corresponds to (x/w y/w), homog coords)
(define (m:*v m vec)
  (assert (or (= (length vec) 2) (= (length vec) 3)))
  (let* ((hv
	  (if (= (length vec) 2)
	      (list (car vec) (cadr vec) 1)
	      (list (car vec) (cadr vec) (caddr vec))))
	 (a (list-dot (car (matrix:vals m)) hv))
	 (b (list-dot (cadr (matrix:vals m)) hv))
	 (c (list-dot (caddr (matrix:vals m)) hv)))
    (list (/ a c) (/ b c))))
  
(define (list-dot l1 l2)
  (fold-right + 0 (map (lambda (ele)
			 (* (car ele) (cadr ele)))
		       (zip l1 l2))))
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
  (let ((a (+ (matrix:ind 0 m1) (matrix:ind 0 m2)))
	(b (+ (matrix:ind 1 m1) (matrix:ind 1 m2)))
	(c (+ (matrix:ind 2 m1) (matrix:ind 2 m2)))
	(d (+ (matrix:ind 3 m1) (matrix:ind 3 m2)))
	(e (+ (matrix:ind 4 m1) (matrix:ind 4 m2)))
	(f (+ (matrix:ind 5 m1) (matrix:ind 5 m2)))
	(g (+ (matrix:ind 6 m1) (matrix:ind 6 m2)))
	(h (+ (matrix:ind 7 m1) (matrix:ind 7 m2)))
	(i (+ (matrix:ind 8 m1) (matrix:ind 8 m2))))
    (%matrix:new (list (list a b c)
		       (list d e f)
		       (list g h i)))))

(define (m:- m1 m2)
  (m:+ m1 (m:negate m2)))

(define (m:print m)
  (pp (matrix:vals m)))
