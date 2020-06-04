#lang slideshow

(provide unit
         unit*
         link-color)

(define link-color "orange")

(define (unit*)
  (define W 300)
  (define H 20)
  (define in (filled-rectangle W H))
  (define out (filled-rectangle W H))
  (values (colorize
           (vc-append 2
                      in
                      (filled-rectangle W (* 4 H))
                      out)
           "gray")
          in
          out))

(define (unit)
  (define-values (u in out) (unit*))
  u)
