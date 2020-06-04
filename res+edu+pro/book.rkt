#lang slideshow
(require racket/draw
         racket/class)

(provide book
         textbook
         textbook-title)

(define cover
  (let ([p (new dc-path%)])
    (send p move-to 0 10)
    (send p line-to 60 0)
    (send p line-to 60 1)
    (send p line-to 1 11)
    (send p line-to 10 20)
    (send p line-to 70 10)
    (send p line-to 70 90)
    (send p line-to 10 100)
    (send p line-to 0 90)
    (send p close)
    p))

(define pages
  (let ([p (new dc-path%)])
    (send p move-to 0 10)
    (send p line-to 60 0)
    (send p line-to 70 10)
    (send p line-to 10 20)
    (send p close)
    p))

(define (book title)
  (define no-pen (make-pen #:style 'transparent))
  (define pages-brush (make-brush #:color "burlywood"))
  (define cover-brush (make-brush #:color "darkred"))
  (define spine-pen (make-pen #:color "red"))

  (define book
    (dc (lambda (dc x y)
          (define old-pen (send dc get-pen))
          (define old-brush (send dc get-brush))
          (send dc set-pen no-pen)

          (send dc set-brush pages-brush)
          (send dc draw-path pages x y)

          (send dc set-brush cover-brush)
          (send dc draw-path cover x y)

          (send dc set-pen spine-pen)
          (send dc draw-line (+ x 10) (+ y 20) (+ x 10) (+ y 100))

          (send dc set-brush old-brush)
          (send dc set-pen old-pen))
        70 100))

  (cc-superimpose book
                  (inset
                   (rotate (scale (rotate (colorize title "white")
                                          (* 0.25 pi))
                                  1 1.2)
                           (* -0.22 pi))
                   10 10 0 0)))

(define textbook-title (t "λ"))

(define (textbook)
  (book textbook-title))
  
(module+ main
  (slide (textbook)))
