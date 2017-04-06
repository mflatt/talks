#lang racket
(require racket/draw
         racket/class)

(define bm (read-bitmap "opengl-logo.png"))
(define w (send bm get-width))
(define h (send bm get-height))
(define bstr (make-bytes (* 4 w h)))
(send bm get-argb-pixels 0 0 w h bstr)

(for ([i (in-range 0 (* 4 w h) 4)])
  (define r (bytes-ref bstr (+ 1 i)))
  (define g (bytes-ref bstr (+ 2 i)))
  (define b (bytes-ref bstr (+ 3 i)))
  (cond
   [(and (>= 250 r) (>= 250 g) (>= 250 b))
    (bytes-set! bstr i (let ([v (- 255 (min r g b))])
                         (inexact->exact (ceiling (sqrt (- (* 255 255) (* (- 255 v) (- 255 v))))))))]
   [else
    (bytes-set! bstr i 0)]))

(send bm set-argb-pixels 0 0 w h bstr)
(send bm save-file "opengl.png" 'png)
