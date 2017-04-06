#lang slideshow
(require slideshow/play
         "in-file.rkt"
         "thesis.rkt")

(provide submodule-split-slides)

(define (submodule-split-slides #:animate? [animate? #t]
                                #:always-arrow? [always-arrow? #f]
                                #:skip-first? [skip-first? #f]
                                #:skip-last? [skip-last? #f]
                                #:wrap [wrap (lambda (n f) (f n))])
  (define title "Phases, Scope, and Submodules")
  (define (split split-n
                 #:split-arrow? [split-arrow? #f])
    (wrap
     split-n
     (lambda (split-n)
       (thesis-module #:split-n split-n
                      #:split-arrow? (or always-arrow? split-arrow?)
                      #:new? #t
                      #:name "continuum"
                      #:wrapping-color "yellow"))))
  (when animate?
    (play-n #:title title split
            #:skip-first? skip-first?
            #:skip-last? skip-last?))
  (when (not always-arrow?)
    (slide #:title title (split 1 #:split-arrow? #t))))

(module+ main
  (submodule-split-slides))
