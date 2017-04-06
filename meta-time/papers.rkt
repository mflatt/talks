#lang slideshow
(require slideshow/play)

(provide papers-slides)

(define (papers-slides)
  (define ywiw (inset (pict->pre-render-pict (bitmap "ywiw.png"))
                      0 (- 50) 0 0))
  (define ywiwa (pict->pre-render-pict (bitmap "ywiwa.png")))

  (define s 0.75)

  (define dy (* client-h 1/3))

  (define ywiwa-x (inset ywiwa
                         (/ (- (pict-width ywiw)
                               (pict-width ywiwa))
                            2)
                         0))

  (define ywiw-x (inset ywiw 0 0 0 
                        (- (+ (pict-height ywiwa-x)
                              dy)
                           (pict-height ywiw))))

  (define (paper p label)
    (rt-superimpose
     (frame (cc-superimpose 
             (colorize (filled-rectangle (pict-width p)
                                         (pict-height p))
                       "white")
             p))
     (colorize (inset (t (~a "[" label "]")) gap-size) "blue")))
  
  (define (papers n)
    (scale 
     (inset
      (lt-superimpose (inset (paper ywiw-x "ICFP'02")
                             (* gap-size (- 1 n))
                             (* dy (- 1 n))
                             0 0)
                      (inset (paper ywiwa-x "GPCE'13")
                             gap-size
                             dy
                             0 0))
      0 60 0 0)
     s))

  (play-n papers))

(module+ main
  (papers-slides))

