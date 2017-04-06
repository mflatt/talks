#lang slideshow
(require slideshow/code
         slideshow/balloon)

(provide runtime-color
         runtime-only-color
         comptime-color
         load-color
         use-color
         new-color
         ref-color
         extra-color
         hilite
         encloud
         adj)

(balloon-enable-3d #f)

(current-keyword-list (list* "define-objc-class"
                             "define-runtime-module-path-index"
                             "class"
                             "new"
                             "define-cstruct"
                             "use" "eval-when"
                             "groceries"
                             "defstx" "define-now-alias" 
                             "define-recently-alias" "define-about-then-alias"
                             (current-keyword-list)))

(define runtime-color "dodgerblue")
(define runtime-only-color "deepskyblue")
(define comptime-color "purple")
(define load-color "forestgreen")
(define use-color "lime")
(define new-color "orange")
(define ref-color use-color)
(define extra-color "yellow")

(define (hilite p 
                #:color [color use-color]
                #:style [style 'rectangle])
  (refocus (cc-superimpose
            (colorize ((if (eq? style 'ellipse)
                           filled-ellipse
                           filled-rectangle)
                       (pict-width p)
                       (pict-height p)) 
                      color)
            p)
           p))

(define (encloud #:color [color runtime-color]
                 #:dh [dh 0]
                 p)
  (refocus (cc-superimpose (let ([p (inset p 4 0)])
                             (colorize
                              (filled-rounded-rectangle (pict-width p) 
                                                        (+ dh (pict-height p))
                                                        5)
                              color))
                           p)
           p))

(define (adj find dx dy)
  (lambda (a b)
    (define-values (x y) (find a b))
    (values (+ x dx) (+ y dy))))
