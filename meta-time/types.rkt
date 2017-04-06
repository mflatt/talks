#lang at-exp slideshow
(require slideshow/code
         slideshow/balloon
         "style.rkt"
         "in-file.rkt"
         "bubble.rkt")

(provide types-slides)

(define (types-slides)
  (define (type-slide #:hilite [hilite values])
    (slide
     #:title "Type Annotations"
     (mk-file #:name "time"
              #:suffix "rkt"
              (scale
               (code
                #,(tt "#lang typed/racket")
                code:blank
                #,(hilite (code (: seconds->hours (Integer -> Integer))))
                (define (seconds->hours secs)
                  (quotient secs (* 60 60)))
                code:blank)
               1))))

  (type-slide)
  (type-slide #:hilite (compose (bubble 'sw "at type-checking time")
                                (hi extra-color))))

(module+ main
  (types-slides))
