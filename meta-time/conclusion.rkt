#lang slideshow
(require slideshow/play
         "thesis.rkt"
         "use-lex-scope.rkt"
         "logo.rkt"
         "config.rkt")

(provide end-slides)

(define (racket-advert)
  (hc-append (* 1 gap-size)
             (scale racket-logo 1/4)
             (vl-append
              (text "Racket" "Optima, Bold" 96)
              (tt "www.racket-lang.org"))))

(define (slower n)
  (* n n))

(define (end-slide #:use-lex? [use-lex? #f]
                   #:mod? [mod? use-lex?]
                   #:split? [split? #f]
                   #:fade-split? [fade-split? #f]
                   #:more? [more? mod?]
                   #:racket? [racket? #f]
                   #:fade-racket? [fade-racket? #f]
                   #:side-racket? [side-racket? #f])
  (define title 
    (cond
     [mod? (if gpce-version?
               "Racket Modules and Submodules Tame Phases"
               "Racket Modules Tame Phases")]
     [more? "∃ More Phases"]
     [else "∃ Phases"]))
  (define advert (racket-advert))
  (define side-advert (scale advert 0.9))
  (define side-space (* 2 gap-size))
  ((if (or fade-split?
           fade-racket?)
       (lambda (f #:title t #:name n) 
         (play-n #:title t #:name n #:skip-first? #t f))
       (lambda (f #:title t #:name n)
         (slide #:title t #:name n (f 0))))
   #:title (let ([plain (titlet "Module")])
             (refocus (cc-superimpose (ghost plain) (titlet title))
                      plain))
   #:name title
   (lambda (n)
     (vc-append
      gap-size
      (let ([m (thesis-module #:declare? mod? #:new? more?
                              #:split-n (if split?
                                            (if fade-split? n 1)
                                            0))])
        (hc-append
         (refocus (hc-append
                   side-space
                   (refocus (hb-append
                             (* -4 gap-size)
                             m
                             ((if use-lex? values ghost)
                              (scale use-lex-scope-balloon 3/4)))
                            m)
                   (cellophane side-advert
                               (if side-racket?
                                   (if fade-racket? (slower n) 1)
                                   0)))
                  m)
         (if side-racket?
             (fade-pict (if fade-racket? n 1)
                        (blank)
                        (inset (ghost side-advert)
                               side-space 0 0 0))
             (blank))))
      (blank)
      ((if racket? values ghost)
       advert)))))

(define (end-slides)
  (end-slide)
  (end-slide #:more? #t)
  (end-slide #:mod? #t
             #:split? gpce-version?
             #:fade-split? #t)
  (when gpce-version?
    (end-slide #:mod? #t
               #:split? #t
               #:side-racket? #t
               #:fade-racket? #t))
  (unless gpce-version?
    (end-slide #:use-lex? #t)
    (end-slide #:use-lex? #t #:racket? #t)))

(module+ main
  (end-slides))
