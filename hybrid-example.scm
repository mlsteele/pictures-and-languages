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
(draw *ur*)

(define (embedded-squares)
  (scale 0.5)
  (square))
