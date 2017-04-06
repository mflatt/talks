#lang slideshow

(provide (all-defined-out))

(define hilite-color "yellow")
(define alt-hilite-color "greenyellow")
(define fail-color "pink")
(define patcol "wheat")
(define tmplcol "palegreen")
(define runtime-color "lightblue")
(define comptime-color "thistle")
(define panel-color "lightblue")

(module+ main
  (define (show c)
    (cond
     [c
      (define p (tt c))
      (define g gap-size)
      (cc-superimpose (colorize (filled-rectangle (+ (pict-width p) g)
                                                  (+ (pict-height p) g))
                                c)
                      p)]
     [else (blank)]))
  (slide
   (table 3
          (map show
               (list hilite-color alt-hilite-color fail-color
                     patcol tmplcol runtime-color 
                     comptime-color panel-color #f))
          cc-superimpose cc-superimpose
          gap-size gap-size)))