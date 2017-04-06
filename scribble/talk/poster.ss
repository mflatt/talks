#lang slideshow
(require scheme/class
         scheme/gui/base)

(provide poster)

(define (poster sz-p rot-p angle)
  (let ([p (new dc-path%)]
        [w (pict-width sz-p)]
        [h (pict-height sz-p)]
        [rw (pict-width rot-p)]
        [rh (pict-height rot-p)])
    (send p move-to 0 0)
    (send p curve-to (/ 2 w) 0 (- w 10) 5 (+ w 1) -2)
    (send p curve-to (- w 2) 10 (+ w 3) (- h 10) (+ w 1) (+ h 1))
    (send p curve-to (* w 2/3) (+ h 3) (* w 1/3) (+ h 3) 0 h)
    (send p close)
    (send p rotate angle)
    (dc (lambda (dc x y)
          (let ([op (send dc get-pen)]
                [ob (send dc get-brush)])
            (send dc set-pen "black" 2 'solid)
            (send dc set-brush "wheat" 'solid)
            (send dc draw-path p x (+ y (* 1.2 h (cos angle))))
            (send dc set-pen op)
            (send dc set-brush ob)))
        rw rh)))

