#lang slideshow
(require "logo.rkt")

(provide title-slide)

(define (title-slide)
  (slide
   (titlet "A Racket Perspective on Research, Education, and Production")
   (inset logo gap-size)
   (vc-append
    (current-line-sep)
    (colorize (bt "Matthew Flatt") "darkblue")
    (colorize (t "University of Utah") "darkred"))))

(module+ main
  (title-slide))


