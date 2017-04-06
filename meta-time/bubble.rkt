#lang at-exp slideshow
(require slideshow/code
         slideshow/balloon
         "style.rkt")

(provide hi
         bubble)

(define (hi col)
  (lambda (p) (encloud p #:color col)))

(define (bubble dir #:scale [s 1.0] . l)
  (lambda (p)
    (refocus
     (pin-balloon (wrap-balloon (scale (apply para #:fill? #f l)
                                       s)
                                dir 0 (case dir
                                        [(n) (- (* 2 gap-size))]
                                        [(s sw se) gap-size]))
                  p
                  p (case dir
                      [(n) cb-find]
                      [(s sw se) ct-find]))
     p)))
