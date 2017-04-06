#lang slideshow 
(require slideshow/code
         "latex.ss")

(provide related-slides)

(define (t/a s a)
  (vc-append 
   (current-line-sep)
   (t s)
   (scale (t (format " (~a)" a)) 0.8)))

(define (enbox #:w [w 1] #:color [color "lightblue"] #:dh [dh 0] s p)
  (let ([p (cc-superimpose
            (colorize (filled-rounded-rectangle (* w (/ client-w 4))
                                                (* gap-size (+ 8 dh)))
                      color)
            p)])
    (refocus (vc-append (* gap-size 3/4)
                        p
                        (scale s 0.60))
             p)))

(define (vc-append* d a b)
  (pin-under (vc-append d a (ghost b))
             b
             lt-find
             b))

(define (related-slide scribble laml)
  (slide
   #:title "Related Work"
   (vc-append*
    5
    (hc-append
     (* 2 gap-size)
     (enbox
      (scribble (hbl-append (tt "#lang ") (code scribble/base)))
      (vc-append gap-size
                 latex
                 (t/a "Skribe" "Gallesio & Serrano")))
     (enbox (scribble (code (require scribble/srcdoc)))
            (vc-append gap-size
                       (t "JavaDoc")
                       (t "...")))
     (enbox (scribble (hbl-append (tt "#lang ") (code scribble/lp)))
            (t/a "WEB" "Knuth")))
    (scribble
     (enbox #:w 3.6 #:color "mediumgoldenrod" 
            (blank) (inset (hbl-append (bt "Scribble") (t " / ") (bt "PLT"))
                           0 
                           (* 1 gap-size) 
                           0 0))))
   (blank)
   (laml (enbox #:w 3.6 #:dh -3 #:color "pink"
                (blank)
                (t/a "LAML, SchemeDoc, LENO" "Nørmark")))))
 

(define (related-slides)
  (related-slide ghost ghost)
  (related-slide values ghost)
  (related-slide values values))
