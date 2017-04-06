#lang slideshow
(provide latex)

(define (rt s) (text s `roman (current-font-size)))
(define (make-latex l a t e x)
  (hbl-append (- (* (pict-width l) 1/3))
              l 
              (inset (lift-above-baseline a (* (pict-height a) 1/4))
                     (- (* (pict-width a) 1/5)) 0 0 0)
              (inset t (* (pict-width t) 1/5) 0)
              (inset (lift-above-baseline e (- (* (pict-height e) 1/5)))
                     (* (pict-width e) 1/7) 0)
              x))
(define latex                 
  (make-latex (rt "L") (scale (rt "A") 0.8) (rt "T") (rt "E") (rt "X")))

