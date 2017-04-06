#lang slideshow
(require "princess.ss"
         "latex.ss")

(provide history-slides)

(define (bad-item . s)
  (colorize (apply item #:bullet (bt "\u21D2") #:width (* 0.8 (current-para-width)) s) "firebrick"))

(define (history-slides)
  (slide
   #:title "Old Documentation"
   (item "Core documentation written in" latex)
   (para #:align 'right "... plus a dozen pre-processors")
   (blank)
   (item "Library documentation written in plain text")
   (para #:align 'right "... plus ad hoc rules for indexing")
   'next
   (blank)
   (blank)
   (hc-append
    (vl-append
     gap-size
     (bad-item "badly organized")
     (bad-item "poorly cross-referenced and linked")
     (bad-item "little tutorial/overview information")
     (bad-item "few examples"))
    (scale (make-princess #:smile? #f) 2.0))))
