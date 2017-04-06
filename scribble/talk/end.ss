#lang slideshow
(require "analogy.ss"
         "castle.ss"
         "movie.ss"
         slideshow/play)

(provide end-slides
         final-end-slide)

(define (white-out n p)
  (refocus
   (cc-superimpose p
                   (cellophane
                    (colorize (filled-rectangle (+ (pict-width p) (* 4 gap-size))
                                                (pict-height p))
                              "white")
                    n))
   p))

(define (transform n a b)
  (define-values (n1 n2) (split-phase n))
  (cc-superimpose (if (= n1 1.0)
                      (ghost a)
                      (white-out n1 a))
                  (if (= n2 0.0)
                      (ghost b)
                      (white-out (- 1 n2) b))))

(define urls
  (scale
   (tt "racket-lang.org")
   0.8))

(define (make-end-slide book-n scribble-n url-n plt-n)
  (vc-append
   gap-size
   (transform book-n (scale castle 0.5) book-icon)
   (let ([p (transform scribble-n (scale million-well 0.5) drscheme-screen)])
     (refocus (hc-append
               gap-size
               p
               (cellophane urls url-n))
              p))
   (transform plt-n (scale kingdom 0.5) plt-langs)))


(define (end-slides)
  (play-n
   #:name "Conclusion"
   make-end-slide))

(define final-end-slide
  (make-end-slide 1.0 1.0 1.0 1.0))
