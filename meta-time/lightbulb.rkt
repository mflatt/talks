#lang racket/base
(require slideshow/pict
         racket/draw
         racket/class
         racket/math)

(provide lightbulb
         bright-lightbulb)

(define (add-line p x1 y1 x2 y2
                  #:width [w 8]
                  #:unstart? [unstart? #f]
                  #:start? [start? (not unstart?)]
                  #:end? [end? #t]
                  #:close? [close? #t]
                  #:ccw? [ccw? #f])
  (define w/2 (/ w 2))
  (if (and (= x1 x2)
           (= y1 y2))
      (send p ellipse (- x1 w/2) (- y1 w/2) w w)
      (let ([a (atan (- y1 y2) (- x2 x1))]
            [a+ (if ccw? + -)]
            [a- (if ccw? - +)])
        (when unstart?
          (define x (+ x1 (* w/2 (cos (a+ a (* pi 3/2))))))
          (define y (- y1 (* w/2 (sin (a+ a (* pi 3/2))))))
          (if (send p open?)
              (send p line-to x y)
              (send p move-to x y)))
        (when start?
          (send p arc (- x1 w/2) (- y1 w/2) w w (a+ a (/ pi 2)) (a+ a (* 3/2 pi)) ccw?))
        (when end?
          (send p arc (- x2 w/2) (- y2 w/2) w w (a- a (/ pi 2)) (a- a (* 3/2 pi)) ccw?))
        (when unstart?
          (send p line-to
                (+ x1 (* w/2 (cos (a+ a (/ pi 2)))))
                (- y1 (* w/2 (sin (a+ a (/ pi 2)))))))
        (when close?
          (send p close)))))

(define (lightbulb #:outline? [outline? #t])
  (let ([bulb (new dc-path%)]
        [screw (new dc-path%)])
    (send bulb move-to 27 100)
    (send bulb line-to 73 100)
    (send bulb line-to 73 75)
    (send bulb arc 5 -15 90 90 (* pi -1/6) (* pi 7/6) #t)
    (send bulb line-to 27 75)
    (send bulb close)
    (add-line screw 30 100 70 100 #:ccw? #f #:close? #f)
    (add-line screw 30 110 70 100 #:ccw? #f #:end? #f)
    (add-line screw 30 120 70 110)
    (add-line screw 30 130 70 120)
    (add-line screw 43 136 57 135 #:end? #f #:close? #f #:ccw? #f)
    (add-line screw 50 135 70 130 #:unstart? #t #:close? #f #:ccw? #f)
    (add-line screw 50 139 50 140 #:start? #f #:ccw? #f)

    (dc (lambda (dc x y)
          (let ([y (+ y 15)]
                [x (- x 5)])
            (define br (send dc get-brush))
            (define pn (send dc get-pen))
            
            (if outline?
                (send dc set-pen (make-pen #:width 1))
                (send dc set-pen (make-pen #:style 'transparent)))
            (send dc set-brush (make-brush #:color "yellow"))
            (send dc draw-path bulb x y)

            (send dc set-brush (make-brush #:color "gray"))
            (send dc draw-path screw x y 'winding)
            
            (send dc set-pen pn)
            (send dc set-brush br)))
        90 160)))

(define (bright-lightbulb [lightbulb (lightbulb)])
  (ct-superimpose (dc (let ([rays (new dc-path%)]
                            [w 6])
                        (add-line #:width w rays 5 75 25 75)
                        (add-line #:width w rays 145 75 165 75)
                        (add-line #:width w rays 85 23 85 5)
                        (add-line #:width w rays 28 20 42 36)
                        (add-line #:width w rays 142 20 128 36)
                        (add-line #:width w rays 26 130 40 114)
                        (add-line #:width w rays 144 130 130 114)
                        (lambda (dc x y)
                          (define br (send dc get-brush))
                          (define pn (send dc get-pen))
                        
                          (send dc set-pen (make-pen #:width 1 #:color "orange"))
                          (send dc set-brush (make-brush #:color "yellow"))
                          
                          (send dc draw-path rays x y)
                        
                          (send dc set-pen pn)
                          (send dc set-brush br)))
                      170 90)
                  (inset lightbulb 0 35 0 0)))
