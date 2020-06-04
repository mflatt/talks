#lang racket
(require racket/draw)

(define bm (read-bitmap "gui-raw-gtk.png"))
(define dc (send bm make-dc))

(define w (send bm get-width))
(define h (send bm get-height))

(define (find x y dx dy)
  (define c1 (make-object color%))
  (define c2 (make-object color%))
  (send dc get-pixel x y c1)
  (send dc get-pixel (+ x dx) (+ y dy) c2)
  (if (> (+ (send c2 red) (send c2 green)(send c2 blue))
         (+ (send c1 red) (send c1 green)(send c1 blue) 50))
      (values x y)
      (find (+ x dx) (+ y dy) dx dy)))

(define-values (lx ly) (find 0 (quotient h 2) 1 0))
(define-values (rx ry) (find (sub1 w) (quotient h 2) -1 0))
(define-values (tx ty) (find (quotient w 2) 0 0 1))
(define-values (bx by) (find (quotient w 2) (sub1 h) 0 -1))

;; For GTK:
;; (set! ty 45)

(let ([c (make-object color%)])
  (for* ([i (in-range 0 w)]
         [j (in-range 0 h)])
    (unless (and (<= lx i rx)
                 (<= ty j by))
      (send dc get-pixel i j c)
      (define a (- 1.0 (/ (+ (send c red) (send c green)(send c blue)) (* 3 255.0))))
      (send c set 0 0 0 a)
      (send dc set-pixel i j c))))

(send bm save-file "gui-gtk.png" 'png)
