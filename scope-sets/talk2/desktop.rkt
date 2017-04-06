#lang at-exp racket/gui
(require slideshow "title-lib.rkt")

(provide plt-desktop)

(define (plt-desktop)
  
  (define (lighten c)
    (let ([f (λ (x) (- 255 (quotient (- 255 x) 2)))])
      (make-object color%
        (f (send c red))
        (f (send c green))
        (f (send c blue)))))
  
  (define title-text-color (send the-color-database find-color "black"))
  (define my-red-color (make-object color% 242 183 183))
  (define my-blue-color (make-object color% 183 202 242))
  (define my-background-color (make-object color% 209 220 248))
  (define my-lambda-color (send the-color-database find-color "white"))
  (define plt-pen-color "black")
  (define plt-pen-style 'transparent)

  (define lighter-lambda-color (lighten my-lambda-color))
  (define lighter-red-color (lighten my-red-color))
  (define lighter-blue-color (lighten my-blue-color))
  (define lighter-background-color (lighten my-background-color))

  
  (define plt-title-background/colors 
    (make-plt-title-background my-red-color
                               my-blue-color
                               my-background-color
                               my-lambda-color
                               plt-pen-color
                               plt-pen-style
                               #:full-size? #t))
  
  plt-title-background/colors)

(module+ main
  (slide (plt-desktop)))
