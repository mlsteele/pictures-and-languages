(load "scratch")
(hybrid-reset!)
(rotate 10)
(scale 0.7)
(line! 0 0 1 1)

(square 0.5)

(hybrid-reset!)
(rotate 90)
(translate 0 0.4)
(rotate -90)
(flip 0)
(line! 0 0 0 0.4)
(line! 0 0 0.03 0.4)

(hybrid-reset!)
(scale 0.5 (lambda _
             (square 0.5)))
(scale 0.5)
(square 0.5)
(draw *ur*)


(hybrid-reset!)
(mirror 45 (lambda _
             (square 0.5)
             (forward 0.5)
             (scale 0.5)
             (square 0.5)))
(draw *ur*)


(define (castle-walls)
  (mirror 90 (lambda _
    (square)
    (translate -0.6 1)
    (scale 0.4)
    (guard castle-walls))))
(hybrid-reset!)
(scale 0.1)
(mirror 90 (lambda _
  (repeat 4 (lambda _
    (castle-walls)
    (translate 2 0)))))
(draw *ur*)


(hybrid-reset!)
(scale 0.3 (lambda _ (circle 1 100)))
(draw *ur*)


(define (tree)
  (circle 1 100)
  (define (fork)
    (mirror 90 (lambda _
                 (translate 1 3)
                 (scale 0.9)
                 (guard tree))))
  (define (up-and-up)
    (save-excursion (lambda _
      (translate 0 3)
      (scale 0.5)
      (guard tree))))
  (if (= 0 (random 3))
    (fork)
    (up-and-up)))
(hybrid-reset!)
(scale 0.1 tree)
(draw *ur*)


(hybrid-reset!)
(define (square2 size)
  (repeat 4 (lambda _
    (line! 0 0 0 size)
    (forward size)
    (rotate 90))))
(color! "black")
(square 100)
(draw *ur*)


(hybrid-reset!)
(repeat 10 (lambda _
  (repeat 6 (lambda _
    ;; inside this inner repeat, the transformation
    ;; is advanced, and that effects the next line
    (rotate 40)
    (color! "red")
    (line! 0 0 0 100)
    (forward 100)
    (rotate -10)
    (color! "blue")
    (line! 0 0 0 100)
    (forward 100)))
  ;; But the out here in the outer repeat, all the
  ;; transformation of the inner repeat is unwound.
  ;; So the inner repeat doesn't have to be responsible
  ;; for returning the would-be turtle to the center again.
  (rotate (/ 360 10))))
(draw *ur*)


(define (swirl decoration)
  (repeat 10 (lambda _
    (repeat 6 (lambda _
      ;; inside this inner repeat, the transformation
      ;; is advanced, and that effects the next line
      (rotate 40)
      (color! "red")
      (line! 0 0 0 100)
      (forward 100)
      (decoration)
      (rotate -10)
      (color! "blue")
      (line! 0 0 0 100)
      (forward 100)))
    (rotate (/ 360 10)))))
(hybrid-reset!)
(swirl (lambda _
  (color! "purple")
  (mirror 80 (lambda _
    (square 50)))))
(draw *ur*)
