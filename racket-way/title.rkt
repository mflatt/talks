#lang slideshow
(require "../scribble/talk/movie.ss")
(provide title-slide)

(define chinese-name? #f)

(define (person s where) 
  (vc-append
   (current-line-sep)
   (colorize (t s) "blue")
   (scale (t where) 0.8)))

(define (make-title-slide the-title the-authors)
  (define title (vc-append
                 (* gap-size 4)
                 the-title
                 plt-bm
                 the-authors))
  (cc-superimpose full-page
                  title))

(define (title-slide the-title)
  (slide (make-title-slide
          the-title
          (person (string-append "Matthew Flatt"
                                 (if chinese-name? "   马晓" ""))
                  "PLT and University of Utah"))))

(module+ main
  (title-slide (titlet "The Racket Way")))
