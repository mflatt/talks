#lang racket/base
(require slideshow
         slideshow/balloon
         racket/draw
         racket/class)

(provide shifted
         brighter-color
         darker-color
         error-para
         enpink
         cellophane-pane
         add-cite)

(define (shifted f dx dy)
  (lambda (p q)
    (let-values ([(x y) (f p q)])
      (values (+ x dx) (+ y dy)))))

(define (brighter-color c s)
  (define (brighter n) (- 255 (quotient (- 255 n) s)))
  (let ([c (if (string? c)
               (send the-color-database find-color c)
               c)])
    (make-color (brighter (send c red))
                (brighter (send c green))
                (brighter (send c blue)))))

(define (darker-color c s)
  (define (darker n) (inexact->exact (round (* n s))))
  (make-color (darker (send c red))
              (darker (send c green))
              (darker (send c blue))))

(define (error-para #:width [width (current-para-width)]
                    . content)
  (parameterize ([current-main-font `(italic . modern)])
    (colorize
     (apply para #:width width content)
     "red")))

(define (enpink p)
  (refocus
   (cc-superimpose (colorize (filled-rectangle (pict-width p) (pict-height p)) "pink")
                   p)
   p))

(define (cellophane-pane p a
                         #:margin [margin 0])
  (cc-superimpose
   p
   (inset (cellophane (colorize (filled-rectangle (+ (pict-width p) (* 2 margin))
                                                  (+ (pict-height p) (* 2 margin)))
                                "white")
                      a)
          (- margin))))

(define (add-cite p s #:page [page full-page])
  (refocus (rb-superimpose
            (cc-superimpose page
                            p)
            (colorize
             (inset (scale (t s) 0.75)
                    (/ gap-size 2))
             "blue"))
           p))
