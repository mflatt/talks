#lang slideshow
(require slideshow/play
         slideshow/code
         racket/runtime-path
         "history.rkt"
         "drracket.rkt"
         "util.rkt")

(provide htdp-slides)

(define-runtime-path htdp-png "logos/htdp-cover-small.png")
(define htdp (scale (bitmap htdp-png) 0.9))

(define-runtime-path emacs-png "emacs.png")
(define emacs (frame (scale (bitmap emacs-png) 0.9)))

(define-runtime-path java-logo-png "logos/java-logo.png")
(define java-logo (bitmap java-logo-png))

(define (htdp-slides)
  (define ca-DrScheme (ca 1995 "HtDP and DrScheme" #:who "Felleisen et al."))
  (define ca-MzScheme (ca 1995 "MzScheme"))

  (define drr-s 0.5)
  (define small-drr (scale dr53-err drr-s))

  (define (impl using #:maybe? maybe?)
    (cc-superimpose
     (ghost dr53-err)
     (refocus
      (hc-append (* 2 gap-size)
                 (rc-superimpose using (ghost (scale java-logo 1/4)))
                 (let ([a (colorize (arrow (* 4 gap-size) 0) "forestgreen")])
                   (cc-superimpose a
                                   ((if maybe? values ghost)
                                    (colorize (scale (text "?" '(bold . roman) (current-font-size)) 1)
                                              "white"))))
                 small-drr)
      small-drr)))

  (as-history
   #:res 0.0
   #:prod 0.0

   (slide
    #:title ca-DrScheme
    #:name "DrScheme"
    htdp))

  (as-history
   #:res 0.0

   (slide
    #:title ca-DrScheme
    #:name "DrScheme"
    #:layout 'tall
    emacs)

   (play-n
    #:title ca-DrScheme
    #:name "DrScheme"
    #:layout 'tall
    #:skip-last? #t
    (lambda (n)
      (cc-superimpose
       (ghost dr53-err)
       (unsmoothed (scale dr53-err (- 1 (* drr-s (fast-middle n))))))))

   (slide
    #:title ca-DrScheme
    #:name "DrScheme"
    #:layout 'tall
    (impl (scale java-logo 1/4) #:maybe? #t)))
   
  (as-history
   #:res 0.0
   #:edu 0.0
   
   (slide
    #:title ca-MzScheme
    #:name "MzScheme"
    #:layout 'tall
    (impl (scale (code (code:blank)) 2) #:maybe? #f)))
   
  (void))
   

(module+ main
  (htdp-slides))

