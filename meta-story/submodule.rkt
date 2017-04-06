#lang slideshow
(require slideshow/play
         "../meta-time/submod.rkt"
         "../meta-time/types.rkt"
         "../meta-time/runtime-paths.rkt"
         "../meta-time/docs.rkt"
         "../meta-time/split.rkt"
         "utils.rkt"
         "lesson.rkt")

(provide submodule-slides)


(define (submodule-slides)
  (test-submod-slides)
  (types-slides)
  (runtime-path-slides)
  (short-doc-slides)
  (submodule-split-slides
   #:wrap (lambda (n f)
            (if (= n 1)
                (add-cite (f 1)
                          "Flatt [GPCE'13]")
                (f n))))
  
  (define inc
    (incidental #:width (* client-w 1/4)
                #:fill? #t
                "Tied to module system"))
  (define inev
    (inevitable #:width (* client-w 1/4)
                #:fill? #t
                "Overlapping but non-nested scopes"))
  
  (define (conclude inev?)
    (define (add-notes p inc?)
      (hc-append
       (* 1.5 gap-size)
       p
       (vc-append
        (* 2 gap-size)
        ((if inc? values ghost) inc)
        ((if inev? values ghost) inev))))
    (submodule-split-slides
     #:animate? (not inev?)
     #:skip-first? #t
     #:always-arrow? (not inev?)
     #:wrap (lambda (n f)
              (define p (f 1))
              (define g-p (ghost p))
              (define g-p2 (ghost p))
              (slide-pict (cc-superimpose
                           g-p
                           (add-notes g-p2 (= n 1)))
                          p
                          g-p g-p2
                          n))))
    
  (conclude #f)
  (conclude #t))

(module+ main
  (submodule-slides))
