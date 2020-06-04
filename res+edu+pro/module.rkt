#lang slideshow
(require slideshow/code
         "util.rkt"
         "history.rkt"
         "color.rkt"
         "unit.rkt"
         "in-file.rkt")

(provide module-slides)

(define (random-units)
  (let-values ([(u in out) (unit*)])
    (define rg (make-pseudo-random-generator))
    (parameterize ([current-pseudo-random-generator rg])
      (random-seed 99))
    (define (r p)
      (define M 40)
      (define-values (dx dy w h c)
        (parameterize ([current-pseudo-random-generator rg])
          (values (random M) (random M)
                  (random (/ (pict-width p) 2)) (random (/ (pict-height p) 2))
                  (random 2))))
      (inset (cc-superimpose p
                             (colorize (filled-rectangle w h)
                                       (list-ref (list rt-color ct-color) c)))
             dx dy (- M dx) (- M dy)))
    (define units
      (list (r u) (r u) (r u) (r u)
            (r u) (r u) (r u) (r u)
            (r u) (r u) (r u) (r u)
            (r u) (r u) (r u) (r u)
            (r u) (r u) (r u) (r u)))
    (scale
     (let ([p (table 4
                     units
                     cc-superimpose cc-superimpose
                     (* 3 gap-size) (* 3 gap-size))])
       (for/fold ([p p]) ([i (in-range (length units))])
         (define pre-j
           (parameterize ([current-pseudo-random-generator rg])
             (random (sub1 (length units)))))
         (define j (modulo (+ i pre-j) (length units)))
         (pin-arrow-line gap-size
                         p
                         (list (list-ref units i) out) cc-find
                         (list (list-ref units j) in) cc-find
                         #:color link-color
                         #:line-width 4
                         #:start-angle (* -1/2 pi)
                         #:end-angle (* -1/2 pi))))
     0.5)))

(define (key-item c txt)
  (define txt-p (t (string-append " " txt)))
  (define s (* 0.8 (pict-height txt-p)))
  (hc-append (colorize (filled-rectangle s s) c)
             txt-p))

(define (add-key p)
  (refocus (ht-append
            gap-size
            (vl-append
             (* 2 (current-line-sep))
             (key-item rt-color "run time")
             (key-item ct-color "compile time"))
            p)
           p))

(define (rt p)
  (background p (blend-color rt-color module-background)))

(define (ct p)
  (background p (blend-color ct-color module-background)))

(define (module-slides)
  (as-history
   #:res 0
   #:edu 0
   (slide
    #:title (ca 2000 "Units Experience")
    #:name "Units Experience"
    (blank)
    (add-key (random-units))))

  (as-history
   #:edu 0

   (define (module-slide #:hilite [hilite values])
     (slide
      #:title (ca 2002 "Modules and Macros")
      #:name "Modules and Macros"
      (leftward
       (in-file
        #:name (tt "slide-seq.rkt")
        (code
         #,(hilite (code #,(tt "#lang") racket))
         (require #,(rt (code slideshow/base))
                  (for-syntax #,(ct (code syntax/parse))))
         code:blank
         (define-syntax (slides stx)
           #,(ct
              (code
               (syntax-case stx ()
                 [(_ id) #'#,(rt (code (void)))]
                 [(_ id e es ...)
                  #'#,(rt (code (let ([id e])
                                  (slide id)
                                  (slides id es ...))))])))))))))

   (module-slide)
   (module-slide #:hilite (lambda (p) (background p "yellow")))))

(module+ main
  (module-slides))

