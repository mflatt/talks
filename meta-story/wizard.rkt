#lang slideshow
(require slideshow/code
         "../meta-time/in-file.rkt"
         "bear.rkt"
         "contributor.rkt"
         "utils.rkt")

(provide wizard-slides)

(define wizard-bear
  (head #:hair 'wizard #:mouth 'goatee))

(define racket/base (code racket/base))
(define bear.rkt (code "bear.rkt"))

(define define-id (code define))
(define bear-id (code bear))

(define (bg p color)
  (refocus
   (cc-superimpose (colorize
                    (filled-rounded-rectangle (+ (pict-width p) 6)
                                              (+ (pict-height p) 6)
                                              8)
                    (brighter-color color 2))
                   p)
   p))

(define (add-macros p #:bindings? [bindings? #f])
  (vc-append
   (* 3 gap-size)
   p
   (scale
    (let* ([color "purple"]
           [color2 "orange"]
           [bg (if bindings?
                   bg
                   (lambda (p c) p))]
           [p 
            (mk-file #:name "team"
                     #:suffix "rkt"
                     (code #,(tt "#lang") #,(bg racket/base color)
                           (require (for-syntax racket/base
                                                syntax/parse)
                                    #,(bg bear.rkt color2))
                           (provide define-bears)
                           code:blank
                           (define-syntax define-bears
                             (syntax-parser
                               [(_ [name style] ...)
                                #'(begin
                                    (#,(bg define-id color) name (#,(bg bear-id color2) #:hair 'style)
                                      ...))]))))]
           [lw 4]
           [p (if bindings?
                  (pin-arrow-line (/ gap-size 2)
                                  p
                                  define-id lt-find
                                  racket/base (shifted lb-find gap-size 0)
                                  #:line-width lw
                                  #:color color)
                  p)]
           [p (if bindings?
                  (pin-arrow-line (/ gap-size 2)
                                  p
                                  bear-id ct-find
                                  bear.rkt cb-find
                                  #:line-width lw
                                  #:color color2)
                  p)])
      p)
    0.75)))

(define (wizard-slides)
  (slide
   (add-macros
    (make-bear-group matthew-bear #:sep 5)))

  (slide
   (add-macros
    (make-bear-group matthew-bear #:sep 5)
    #:bindings? #t))
  
  (slide
   (add-macros
    #:bindings? #t
    (make-bear-group
     wizard-bear
     #:sep 5
     #:filter (lambda (b) wizard-bear)))))

(module+ main
  (wizard-slides))
