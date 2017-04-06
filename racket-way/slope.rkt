#lang slideshow
(require "as-file.rkt"
         slideshow/code)
(provide slope-slides)

(define green "forestgreen")
(define dark-blue "darkblue")

(define base
  (blank (* client-w 0.6)
         (* client-h 0.6)))

(define (add-arrow base find-dest lbl)
  (define p (pin-arrow-line gap-size
                            base
                            base lb-find
                            base find-dest
                            #:color "darkgray"
                            #:line-width 5))
  (lbl p))

(define (label s)
  (inset (para #:fill? #f 
               #:width (* gap-size 8)
               #:align 'right
               s)
         (/ gap-size 3)))

(define like-racket
  (as-file #f
           (scale
            (code
             (define north
               (verb (list 'north 'n) 
                     "go north" 
                     #f))
             ....)
            0.6)))

(define unlike-racket
  (as-file #f
           (scale
            (vl-append
             (current-line-sep)
             (tt*
              "===VERBS===")
             (colorize
              (tt "north, n")
              dark-blue)
             (colorize
              (tt "\"go north\"")
              green)
             (tt " ")
             (tt "...."))
            0.6)))

(define (topify p)
  (inset p
         0 (- gap-size (pict-height p))
         0 0))

(define (centerify p)
  (inset p
         (- (/ (pict-width p) 2))
         0
         (- (/ (pict-width p) 2))
         0))

(define (slope-slides #:lisp-line? [lisp-line? #t]
                      #:non-extensible-label [non-extensible-label "non-extensible"]
                      #:like-racket [like-racket-prog like-racket]
                      #:unlike-racket [unlike-racket-prog unlike-racket])
  (for/fold ([old-past null]) ([step (append '(base langs nonext)
                                             (if lisp-line? '(lisp) null)
                                             '(racket dots))])
    (define past (cons step old-past))
    (slide
     #:title "Creating a Language"
     (let* ([sep gap-size]
            [base (add-arrow
                   base
                   lt-find
                   (lambda (p)
                     (refocus
                      (ht-append
                       sep
                       (inset
                        (label "implementation challenge")
                        0 (* 2 gap-size) 0 0)
                       p)
                      p)))]
            [add-plots (lambda (p)
                         (define lw 3)
                         (define ah (/ gap-size 2))
                         (define nonext-find (lambda (p p2)
                                               (define-values (x y) (rt-find p p2))
                                               (values (- x (* 10 gap-size)) y)))
                         (define lisp-find rt-find)
                         (define racket-find (lambda (p p2)
                                               (define-values (x y) (rt-find p p2))
                                               (values (+ x gap-size) (+ y (* 12 gap-size)))))
                         (define (add-label what p find name color [prop 1/2])
                           (define-values (x1 y1) (lb-find p p))
                           (define-values (x2 y2) (find p p))
                           (if (memq what past)
                               (pin-over p 
                                         (+ (* x1 (- 1 prop)) (* x2 prop))
                                         (+ (* y1 (- 1 prop)) (* y2 prop))
                                         (colorize (t name) color))
                               p))
                         (define (add-dots p)
                           (define N 7)
                           (define s (/ gap-size 2))
                           (define-values (x1 y1) (lb-find p p))
                           (define-values (x2 y2) (racket-find p p))
                           (for/fold ([p p]) ([i (in-range (add1 N))])
                             (define prop (* 0.95 (/ i N)))
                             (pin-over p 
                                       (+ (* x1 (- 1 prop)) (* x2 prop))
                                       (+ (* y1 (- 1 prop)) (* y2 prop))
                                       (inset (colorize (filled-ellipse s s) "blue")
                                              (- (/ s 2))
                                              (- (/ s 2))))))
                         (let* ([p (if (memq 'nonext past)
                                       (pin-arrow-line ah
                                                       p
                                                       p lb-find
                                                       p nonext-find
                                                       #:color "red"
                                                       #:line-width lw)
                                       p)]
                                [p (if (memq 'lisp past)
                                       (pin-arrow-line ah
                                                       p
                                                       p lb-find
                                                       p lisp-find
                                                       #:color "purple"
                                                       #:line-width lw
                                                       #:start-angle (/ pi 7)
                                                       #:end-angle (/ pi 3)
                                                       #:start-pull 1/2
                                                       #:end-pull 1/2)
                                       p)]
                                [p (if (memq 'racket past)
                                       (pin-arrow-line ah
                                                       p
                                                       p lb-find
                                                       p racket-find
                                                       #:color "blue"
                                                       #:line-width lw)
                                       p)]
                                [p (add-label 'nonext p nonext-find non-extensible-label "red")]
                                [p (add-label 'lisp p lisp-find "Lisp" "purple" 3/4)]
                                [p (add-label 'racket p racket-find "Racket" "blue")])
                           (if (memq 'dots past)
                               (add-dots p)
                               p)))])
       (inset
        (add-arrow base
                   rb-find
                   (lambda (p)
                     (vr-append
                      (add-plots
                       (if (memq 'langs past)
                           (refocus (vr-append
                                     sep
                                     (refocus (vl-append sep p (centerify like-racket-prog))
                                              p)
                                     (centerify unlike-racket-prog))
                                    p)
                           p))
                      (inset (label "language variation")
                             0 0 (+ gap-size (/ (pict-width unlike-racket-prog) 2)) 0))))
        (* 4 gap-size) 0 0 0)))
    past))

(module+ main
  (slope-slides))
