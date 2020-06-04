#lang slideshow
(require slideshow/code
         racket/runtime-path
         racket/draw
         (for-syntax racket/base)
         "util.rkt"
         "history.rkt"
         "color.rkt")

(provide scope-slides)

(define-runtime-path strange-loop-jpg "logos/strange-loop.jpg")
(define strange-loop (bitmap strange-loop-jpg))

(define colors
  (vector "lightcoral"
          "deepskyblue"
          "gold"
          (make-color 0 220 0)))

(define-syntax-rule (bg n b ...)
  (background (code b ...)
              (vector-ref colors (sub1 n))))

(define (scope-demo)
  (let-syntax ([let* (make-code-transformer
                      (lambda (stx)
                        (if (identifier? stx)
                            #`(bg 4 let)
                            #f)))])
    (code
     (define #,(bg 1 x) 1)
     #,(bg 1
           code:blank
           (let ([#,(bg 2 x) x])
             #,(bg 2
                   (λ (#,(bg 3 y))
                     #,(bg 3
                           (let* ([#,(bg 4 x) y])
                             (#,(bg 4 if) #,(bg 4 x) #,(bg 4 x) x))))))))))

(define (tint-box color p label suffix)
  (let* ([p (inset p (/ gap-size 2))]
         [p (cc-superimpose
             (frame (cellophane
                     (colorize (filled-rectangle (pict-width p) (pict-height p))
                               color)
                     0.3)
                    #:color color)
             p)])
    (vr-append
     (let ([lbl (inset (hbl-append
                        (text label `modern (current-font-size))
                        (text suffix `(bold . modern) (current-font-size)))
                       5)])
       (cc-superimpose (colorize (filled-rectangle (pict-width lbl) (pict-height lbl))
                                 file-tab-color)
                       lbl))
     p)))

(define (convert-desc)
  (define c-code (tint-box c-color (blank 300 200) "expander." "c"))
  (define rkt-code (tint-box racket-color (blank 300 200) "expander." "rkt"))
  
  (leftward
   (ht-append (* 2 gap-size)
              c-code
              (hc-append
               (* 2 gap-size)
               (colorize (arrow gap-size 0) "forestgreen")
               rkt-code))))

(define (scope-slides)
  (define macros-and-scope (ca 2016 "Macros and Scope"))
  (define name  "Macros and Scope")

  (as-history
   #:edu 0
   (slide
    #:title macros-and-scope
    #:name name
    (scope-demo)))

  (as-history
   #:res 0
   #:edu 0
   (slide
    #:title macros-and-scope
    #:name name
    (convert-desc)))

  (as-history
   #:res 0
   (slide
    #:title macros-and-scope
    #:name name
    (let ([p (convert-desc)])
      (refocus (hb-append p
                          (let ([i (scale strange-loop 0.25)])
                            (inset i (* -2/3 (pict-width i)) 0 0 0)))
               p)))))

(module+ main
  (scope-slides))

