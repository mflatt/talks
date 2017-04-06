#lang slideshow

(provide learning-slides)

(define (cite #:bullet b title . where)
  (vl-append
   (current-line-sep)
   (item #:bullet b (format "``~a''" title))
   (apply item #:align 'right #:bullet (ghost b) where)))

(define green (colorize (filled-ellipse gap-size gap-size) "green"))
(define blue (colorize (filled-rectangle gap-size gap-size) "blue"))
(define black (let* ([p (colorize (filled-rectangle gap-size gap-size) "black")]
                     [p (rotate p (/ pi 4))]
                     [p (scale p 2/3 1)]
                     [p (hc-append p p)])
                (refocus (rc-superimpose p (ghost blue))
                         blue)))

(define (learning-slides)
  (slide
   #:title "Learning More"
   (item #:bullet green (tt "http://docs.racket-lang.org"))
   (cite #:bullet green
         "Fear of Macros"
         (scale (tt "http://www.greghendershott.com/fear-of-macros/") 0.75)
         "Hendershott")
   (blank)
   (cite #:bullet blue
         "Composable and Compilable Macros"
         "Flatt, ICFP'02")
   (cite #:bullet black
         "Macros that Work Together"
         "Flatt, Culpepper, Darais, and Findler, JFP'12")))
