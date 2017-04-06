#lang slideshow
(require scheme/class
         scheme/gui/base
         "style.rkt")

(provide castle
         window-path)

(define-values (castle window-path)
 (let ([p (new dc-path%)]
       [p2 (new dc-path%)]
       [p3 (new dc-path%)]
       [window (new dc-path%)]
       [flag (new dc-path%)])
   (send p move-to 0 20)
   (send p line-to 0 -40)
   (send p line-to -5 -40)
   (send p line-to 15 -60)
   (send p line-to 35 -40)
   (send p line-to 30 -40)
   (send p line-to 30 -35)
   (send p line-to 40 -35)
   (send p line-to 40 -75)
   (send p line-to 35 -75)
   (send p line-to 75 -115)
   (send p line-to 115 -75)
   (send p line-to 110 -75)
   (send p line-to 110 -35)
   (send p line-to 120 -35)
   (send p line-to 120 -40)
   (send p line-to 115 -40)
   (send p line-to 135 -60)
   (send p line-to 155 -40)
   (send p line-to 150 -40)
   (send p line-to 150 20)
   (send p line-to 100 20)
   (send p curve-to 100 -30 80 -30 75 -30)
   (send p curve-to 70 -30 50 -30 50 20)
   (send p close)

   (send window move-to 100 20)
   (send window curve-to 100 -30 80 -30 75 -30)
   (send window curve-to 70 -30 50 -30 50 20)
   (send window close)
   (send window translate 45 154)

   (send p2 move-to -20 40)
   (send p2 line-to -20 -60)
   (send p2 line-to -25 -60)
   (send p2 line-to -25 -75)
   (send p2 line-to -20 -75)
   (send p2 line-to -20 -70)
   (send p2 line-to -15 -70)
   (send p2 line-to -15 -75)
   (send p2 line-to -10 -75)
   (send p2 line-to -10 -70)
   (send p2 line-to -5 -70)
   (send p2 line-to -5 -75)
   (send p2 line-to 0 -75)
   (send p2 line-to 0 -70)
   (send p2 line-to 5 -70)
   (send p2 line-to 5 -75)
   (send p2 line-to 10 -75)
   (send p2 line-to 10 -70)
   (send p2 line-to 15 -70)
   (send p2 line-to 15 -75)
   (send p2 line-to 20 -75)
   (send p2 line-to 20 -60)
   (send p2 line-to 15 -60)
   (send p2 line-to 15 40)
   (send p2 close)
   (send p2 scale 1.5 1.5)

   (send p3 move-to 5 -20)
   (send p3 line-to 5 -100)
   (send p3 line-to 0 -100)
   (send p3 line-to 75 -175)
   (send p3 line-to 150 -100)
   (send p3 line-to 145 -100)
   (send p3 line-to 145 -20)
   ; (send p3 line-to 100 -25)
   (send p3 close)
   (send p3 scale 1.0 1.75)

   (send flag move-to 0 0)
   (send flag curve-to 10 0 0 10 10 10)
   (send flag curve-to 20 10 10 20 20 20)
   (send flag curve-to 19 20 20 30 10 30)
   (send flag curve-to 1 30 10 20 1 20)
   (send flag line-to 1 40)
   (send flag line-to 0 40)
   (send flag close)
   (send flag scale 3.0 1.0)

   (values
    (dc (lambda (dc x y)
          (let ([op (send dc get-pen)]
                [ob (send dc get-brush)]
                [x (+ x 45)]
                [y (+ y 344)])
            (send dc set-pen "black" 2 'solid)
            (send dc set-brush (if 3-D? "red" "pink") 'solid)
            (send dc draw-path flag (+ x 74) (- y 340))
            (define (wall-brush x y)
              (send dc set-brush
                    (make-brush #:gradient (make-object linear-gradient%
                                                        x (- y 100)
                                                        (+ x 40) (- y 60)
                                                        (list (list 0 (make-object color% "blue"))
                                                              (list 1.0 (make-object color% "mediumblue")))))))
            (if 3-D?
                (wall-brush x y)
                (send dc set-brush "darkorchid" 'solid))
            (send dc draw-path p3 x y)
            (define (tower-brush x y)
              (define b
                (make-brush #:gradient (make-object linear-gradient%
                                                    (- x 25) y
                                                    (+ x 15) y
                                                    (list (list 0 (make-color 100 100 100))
                                                          (list 0.5 (make-color 150 150 150))
                                                          (list 1.0 (make-color 100 100 100))))))
              (send dc set-brush b))
            (if 3-D?
                (tower-brush (- x 10) y)
                (send dc set-brush "blue" 'solid))
            (send dc draw-path p2 (+ x -5) (- y 40))
            (when 3-D?
              (tower-brush (+ x 160) y))
            (send dc draw-path p2 (+ x 160) (- y 40))
            (if 3-D?
                (wall-brush x (+ y 100))
                (send dc set-brush "purple" 'solid))
            (send dc draw-path p x y)
            (send dc set-brush "white" 'solid)
            (send dc draw-path window (- x 45) (- y 344))
            (send dc set-pen op)
            (send dc set-brush ob)))
        236 366)
    window)))

