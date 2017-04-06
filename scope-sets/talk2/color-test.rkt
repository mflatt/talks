#lang slideshow
(require slideshow/balloon
         "code.rkt"
         "code-sequence.rkt")

(define p
 (scope-example
  (code
   #,(encolors (tt " ") scope1) #,(colorize (tt "x") scope1/text)
   #,(encolors (tt " ") scope2) #,(colorize (tt "x") scope2/text)
   #,(encolors (tt " ") scope3) one
   #,(encolors (tt " ") scope4) 2
   #,(encolors (tt " ") scope5) (code:comment "three"))))

(slide
 (pin-balloon (scope-balloon (t "check")
                             #:code? #f
                             #:spike 'nw)
              p
              p rb-find))

              
