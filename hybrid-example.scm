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
