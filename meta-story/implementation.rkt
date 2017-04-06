#lang slideshow
(provide make-implementation)

(define region-width (* 2/3 client-w))
(define region-height (* 0.4 client-h))

(define (region name color content #:scale s #:align align)
  (lt-superimpose
   (align (colorize (filled-rectangle region-width (* s region-height))
                    color)
          (apply hc-append gap-size content))
   (inset (t name) 5)))

(define (make-implementation #:top-name [top-name "Scheme"]
                             #:bottom-name [bottom-name "C/C++"]
                             #:top top-content
                             #:bottom bottom-content
                             #:bottom-scale [bottom-scale 1]
                             #:top-align [top-align cc-superimpose]
                             #:bottom-align [bottom-align cc-superimpose]
                             #:sep-show [sep-show values]
                             #:bottom-show [bottom-show values])
  (vc-append
   (region top-name
           "lightblue"
           top-content
           #:scale 1
           #:align top-align)
   (sep-show
    (linewidth 3 (hline region-width 3 #:segment 20)))
   (bottom-show
    (region bottom-name
            "pink"
            bottom-content
            #:scale bottom-scale
            #:align bottom-align))))
