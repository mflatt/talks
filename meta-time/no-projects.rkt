#lang slideshow
(require slideshow/code
         "in-file.rkt")

(provide no-projects-slides)

(define drracket.png
  (scale (bitmap "drracket.png") 0.8))
(define the-arrow (cc-superimpose
                   (arrow (+ (* 2 gap-size) 2) (* pi -2/3))
                   (colorize (arrow (* 2 gap-size) (* pi -2/3))
                             "yellow")))

(define (add-arrow pos)
  (define p (inset drracket.png (* 2 gap-size)))
  (list
   (refocus (lt-superimpose
             p
             (inset the-arrow
                    (+ gap-size (* pos (pict-width drracket.png)))
                    (* 2.5 gap-size)
                    0 0))
            drracket.png)))

(define (no-projects-slides)
  (slide
   #:title "No Project Files"
   (mk-file #:name "movie"
            #:suffix "rkt"
            (scale
             (code
              #,(tt "#lang") racket code:blank
              ....)
             0.8))
   'alts
   (list
    (list (para "To run:")
          (tt "% racket movie.rkt"))
    (list (para "To compile:")
          (tt "% raco make movie.rkt"))
    (list (para "To create an executable:")
          (tt "% raco exe movie.rkt"))
    (list (para "To use the IDE:")
          (tt "% drracket movie.rkt")
          'alts
          (cons
           (list drracket.png)
           (map add-arrow '(85/100 55/100 4/10 3/4)))))))

(module+ main
  (no-projects-slides))


