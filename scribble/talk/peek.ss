#lang slideshow
(require "princess.ss"
         "movie.ss"
         slideshow/play
         scheme/math)

(provide peek-slides)

(define p-width (pict-width (scale (make-princess) 2.0)))

(define (peek-slides final-end-slide 
                     #:wave? [wave? #t]
                     #:smile? [smile? #t]
                     #:balloon [balloon #f])
  (play-it
   #:name "Wave"
   `([10 0.05]
     [,(if wave? 40 0) 0.05])
   (lambda (peek-n wave-n)
     (define (modulate n)
       (let loop ([n n])
         (if (n . > . 2.0)
             (loop (- n 2.0))
             (if (< n 1.0)
                 n
                 (- 2.0 n)))))
     (define princess
       (scale
        (make-princess #:rotate (* peek-n (/ pi -5))
                       #:smile? smile?
                       #:front-arm-angle (if wave?
                                             (+ (* pi 4/10)
                                                (* (* pi 2/10) 
                                                   (if #t
                                                       (modulate (* 12 wave-n))
                                                       (sin (* (modulate (* 4 wave-n)) pi)))))
                                             0))
        2.0))
     (clip-to-screen
      (pin-over
       (cc-superimpose full-page
                       final-end-slide)
       (+ (- (* p-width 1.5)) (* p-width 1.7 peek-n))
       0
       (if (and balloon
                (= peek-n 1))
           (balloon princess)
           princess))))))
