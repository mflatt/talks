#lang slideshow
(require "logo.rkt"
         "tree.rkt")

(provide title-slide
         part)

(define (part-text s #:size [size 48])
  (parameterize ([current-main-font '(italic . "Zapfino")]
                 [current-font-size size])
    (para #:align 'center s)))

(define sep
  (let ([w (* client-w 0.2)])
    (tree-pict (branch 0 0 (list
                            (branch (* -1/2 pi) w
                                    (branch 0 w (curly gap-size #t)))
                            (branch (* 1/2 pi) w
                                    (branch 0 w (curly gap-size #f))))))))

(define (part s)
  (slide
   #:name (format "-- ~a --" s)
   (inset sep
          0 (- gap-size) 0 gap-size)
   (part-text s)
   (rotate sep pi)))

(define (title-slide)
  (slide
   (part-text "How Racket went Meta")
   racket-logo
   (blank)
   (vc-append
    (current-line-sep)
    (text "Matthew Flatt" (current-main-font) 32)
    (text "PLT and University of Utah" (current-main-font) 24))))

(module+ main
  (title-slide)
  (part "Goldilocks and the Three Bears")
  (part "A Tale of Two Languages")
  (part "The Importance of Being Some Specific Name")
  (part "A Funny Thing Happened on the Way to the Expansion"))
