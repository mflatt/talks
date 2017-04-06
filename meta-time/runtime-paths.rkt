#lang at-exp slideshow
(require slideshow/code
         slideshow/balloon
         "style.rkt"
         "in-file.rkt"
         "bubble.rkt")

(provide runtime-path-slides)

(define (runtime-path-slides)
  (define (type-slide #:hilite [hilite values])
    (slide
     #:title "Declaring Extra Run-Time Files"
     (mk-file #:name "big-ben"
              #:suffix "rkt"
              (scale
               (code
                code:blank
                (define-runtime-path clock-image #,(hilite (code "clock.png")))
                code:blank
                (define (seconds->clock-image secs)
                  (add-clock-hands (read-bitmap clock-image)
                                   secs))
                code:blank
                ...
                code:blank)
               1))))

  (type-slide)
  (type-slide #:hilite (compose (bubble 'se "at packaging time")
                                (hi extra-color))))

(module+ main
  (runtime-path-slides))
