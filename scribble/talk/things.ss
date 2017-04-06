#lang slideshow
(require scheme/class
         scheme/gui/base)

(define ice-cream
  (inset
   (scale
    (let ([tri (new dc-path%)])
      (send tri move-to 0 0)
      (send tri line-to 5 -20)
      (send tri line-to -5 -20)
      (send tri close)
      (dc (lambda (dc x y)
            (let ([op (send dc get-pen)]
                  [ob (send dc get-brush)])
              (send dc set-pen "black" 1 'solid)
              (send dc set-brush "pink" 'solid)
              (send dc draw-ellipse x y 10 10)
              (send dc set-brush "white" 'solid)
              (send dc draw-ellipse x (+ y 8) 10 10)
              (send dc set-brush "brown" 'solid)
              (send dc draw-ellipse x (+ y 16) 10 10)
              (send dc set-brush "tan" 'solid)
              (send dc draw-path tri (+ x 5) (+ y 44))
              (send dc set-pen op)
              (send dc set-brush ob)))
          10 44))
    2.0)
   20 0))

(define fishbowl
  (let ([bowl (new dc-path%)])
    (send bowl move-to 5 0)
    (send bowl line-to 8 3)
    (send bowl curve-to 4 3 0 17 0 20)
    (send bowl curve-to 0 35 8 40 10 40)
    (send bowl line-to 20 40)
    (send bowl curve-to 22 40 30 35 30 20)
    (send bowl curve-to 30 17 26 3 22 3)
    (send bowl line-to 25 0)
    (send bowl close)
    (send bowl scale 2.2 1.5)
    (send bowl scale 1.2 1.2)
    (cc-superimpose
     (dc (lambda (dc x y)
           (let ([op (send dc get-pen)]
                 [ob (send dc get-brush)])
             (send dc set-pen "black" 2 'solid)
             (send dc set-brush "skyblue" 'solid)
             (send dc draw-path bowl x y)
             (send dc set-pen op)
             (send dc set-brush ob)))
         (* 30 2.2 1.2) (* 1.5 1.2 40))
     (inset (standard-fish 30 25 #:color "gold") 4 3 0 0))))

(define diamond
  (let ([p (new dc-path%)])
    (let ([x1 5]
          [y1 0]
          [x2 0]
          [y2 5]
          [x3 8]
          [y3 8]
          [x4 20]
          [y4 0]
          [x5 17]
          [y5 8]
          [x6 25]
          [y6 5]
          [x7 12.5]
          [y7 20]
          [s 3]
          [p (new dc-path%)])
      (send p move-to x2 y2)
      (send p line-to x1 y1)
      (send p line-to x4 y4)
      (send p line-to x6 y6)
      (send p line-to x7 y7)
      (send p close)
      (send p scale s s)
      (dc (lambda (dc x y)
           (let ([op (send dc get-pen)]
                 [ob (send dc get-brush)]
                 [l (lambda (a b c d)
                      (send dc draw-line (+ (* s a) x) (+ (* s b) y) (+ (* s c) x) (+ (* s d) y)))])
             (send dc set-pen "black" 2 'solid)
             (send dc set-brush "white" 'solid)
             (send dc draw-path p x y)
             (l x3 y3 x2 y2)
             (l x3 y3 x5 y5)
             (l x5 y5 x6 y6)
             (l x7 y7 x4 y4)
             (l x7 y7 x1 y1)
             (send dc set-pen op)
             (send dc set-brush ob)))
          75 60))))

(provide fishbowl
         ice-cream
         diamond)

