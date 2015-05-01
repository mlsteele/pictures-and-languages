;;; Collection of logo language examples.

(to (ngon sides size)
  (repeat sides
    (fd size)
    (rt (/ 360 sides))))
(to (at-corner)
  (ngon 4 10))
(to (ngon-decorated sides size decoration)
  (repeat sides
    (decoration)
    (fd size)
    (rt (/ 360 sides))))
(ngon-decorated 5 20 at-corner)



(to (square size)
  (repeat 4
    (fd size)
    (rt 90)))
(to (spiral n)
  (square (/ n 4))
  (fd (/ n 20))
  (rt 5)
  (limit n 1)
  (spiral (- n 1)))



(to (ngon sides size)
  (repeat sides
    (fd size)
    (rt (/ 360 sides))))
(to (at-corner)
  (ngon 4 10))
(to (ngon-decorated sides size decoration)
  (repeat sides
    (decoration)
    (fd size)
    (rt (/ 360 sides))))
(ngon-decorated 5 20 at-corner)
