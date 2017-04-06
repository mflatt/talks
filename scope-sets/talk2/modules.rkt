#lang slideshow
(require slideshow/code
         "code.rkt")

(provide modint-slides)

(define bg1 scope3)
(define bg2 scope1)

(define vfactor 1.2)

(define (in-file name content file-background)
  (define color file-background)
  (define border-color "black")
  (define name-box
    (if name
        (inset (scale (if (string? name)
                          (tt name)
                          name)
                      0.8)
               6 3)
        (blank)))
  (define background
    (colorize (filled-rectangle (+ (pict-width content) (current-font-size))
                                (+ (pict-height content) (current-font-size)))
              color))
  (define c
    (cc-superimpose
     (frame background)
     content))
  (define r
    (if name
        (let ([n name-box])
          (define f
            (cc-superimpose (frame
                             (colorize (filled-rectangle (pict-width n)
                                                         (pict-height n))
                                       "beige"))
                            n))
          (vr-append (inset f (min 0 (- (pict-width c) (pict-width f))) 0 0 0)
                     c))
        c))
  r)

(define (module-slide use-class)
  (slide
   ;; #:title "Modules"
   (in-file "class.rkt"
            (code
             (provide class)
             code:blank
             (define-syntax class
               .... extract-method ....)
             code:blank
             (define extract-method ....))
            bg1)
   (blank)
   (in-file "gui.rkt"
            (code
             (require "class.rkt")
             code:blank
             (define window
               #,use-class))
            bg2)))

(define extract-method (code ... #,(encolors (code extract-method) bg1) ...))

(define (intdef-slide expand g)
  (slide
   ;; #:title "Mutually Recursive Definition Contexts"
   (code (define-syntax-rule (define-thunk id rhs)
           (define id (lambda () rhs)))
         code:blank
         #,expand
         code:blank
         ....)
   (blank)
   (blank)
   (g (para #:fill? #f "Scope for use-site as well as macro-introduced"))))

(define (modint-slides)
  (module-slide (rbl-superimpose (code (class ....))
                                 (ghost extract-method)))
  (module-slide extract-method)

  (intdef-slide (code (define-thunk x y)) ghost)
  (intdef-slide (code (#,(encolors (code define) scope4) x (#,(encolors (code lambda) scope4) () #,(encolors (code y) scope2)))) values))

(module+ main
  (modint-slides))
