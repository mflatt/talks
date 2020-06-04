#lang slideshow
(require racket/draw)

(provide gear)

(define (gear #:n [n 10]
              #:rotate [rotate 0]
              #:size [size 100]
              #:ratio [ratio 0.8]
              #:hole-ratio [hole-ratio 0.2]
              #:color [color "darkgray"])
  (define step (/ pi n))
  (define p (new dc-path%))
  (define adj (/ (* (- 1 ratio) 2 pi) (* 2 2 n)))
  (for ([i (in-range n)])
    (send p arc 0 0 size size (+ (* i 2 step) rotate adj) (- (+ (* i 2 step) step rotate) adj))
    (send p arc
          (* (- 1 ratio) 1/2 size) (* (- 1 ratio) 1/2 size)
          (* ratio size) (* ratio size)
          (+ (* i 2 step) step rotate) (+ (* (add1 i) 2 step) rotate)))
  (send p close)
  (send p ellipse
        (* (- 1 hole-ratio) 1/2 size) (* (- 1 hole-ratio) 1/2 size)
        (* hole-ratio size) (* hole-ratio size))
  (define no-pen (make-pen #:style 'transparent))
  (define brush (make-brush #:color color))
  (dc (lambda (dc x y)
        (define old-pen (send dc get-pen))
        (define old-brush (send dc get-brush))
        (send dc set-pen no-pen)
        (send dc set-brush brush)
        (send dc draw-path p x y)
        (send dc set-pen old-pen)
        (send dc set-brush old-brush))
      size size))
