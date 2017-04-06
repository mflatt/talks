#lang slideshow
(require "code.rkt")

(provide terminology-slides)

(define (terminology-slide show-set show-syntax show-url
                           #:slide [slide slide])
  (define (S p) (scale p 1.4))
  (define tbl
   (let ([p (ghost (tt "x"))]
         [eql (S (t "="))])
     (table
      3
      (list
       (let ([b (blank (* (pict-width p) 12)
                       (* (pict-height p) 4))])
         (inset (vc-append (inset (encolors p scope1) 0 0 (* 3 (pict-width p)) 0)
                           (encolors b
                                     scope1))
                0 (- (* 4 (pict-height p))) 0 0))
       eql
       (S (t "scope"))
       
       (show-set (encolors p scope1 scope2 scope4))
       (show-set eql)
       (show-set (S (t "scope set")))
       
       (show-syntax (cc-superimpose (encolors p scope1 scope2 scope4)
                                    (tt "x")))
       (show-syntax eql)
       (show-syntax (S (t "syntax object"))))
      cc-superimpose cc-superimpose
      (* 2 gap-size) (* 4 gap-size))))
  (slide
   (vc-append
    gap-size
    (blank)
    (refocus
     (vc-append (* 4 gap-size)
                tbl
                (show-url (tt "http://www.cs.utah.edu/plt/scope-sets/")))
     tbl))))
  
(define (terminology-slides #:url? [url? #f]
                            #:complete-slide [complete-slide slide]
                            #:extra-slides [extra-slides void])
  (terminology-slide ghost ghost ghost)
  (terminology-slide values ghost ghost)
  (terminology-slide values values ghost #:slide complete-slide)
  (when url?
    (terminology-slide values values values))
  (terminology-slide values values ghost #:slide extra-slides))

(module+ main
  (terminology-slides))
