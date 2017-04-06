#lang slideshow
(require slideshow/play
         "logo.rkt"
         "contributor.rkt"
         "utils.rkt"
         "lesson.rkt")

(provide plot-slides
         
         overall-incidental+inevitable-slides)

(define (bigt s)
  (scale (rt s) 4))

(define (lvc-append a b)
  (vc-append a
             (inset b (/ (- (pict-width a) (pict-width b)) 2) 0)))

(define plot-arrow
  (let ([p (hc-append (* 6 gap-size)
                      (lvc-append (bt "Lisp")
                                  (t "macros"))
                      (lvc-append (bt "Scheme")
                                  (t "macros"))
                      (lvc-append (bt "Racket")
                                  (t "macros"))
                      (blank))])
    (inset
     (pin-arrow-line 250
                     p
                     p lc-find
                     p rc-find
                     #:line-width 100
                     #:color "lightblue"
                     #:under? #t)
     0 gap-size)))

(define-values (all-bears one-bear)
  (let ([mk (lambda (m) (inset (make-bear-group m)
                          0 0 (* client-w 1/3) 0))])
    (values (mk matthew-bear)
            (pin-over (cellophane-pane (mk (ghost matthew-bear)) 0.5
                                       #:margin gap-size)
                      matthew-bear lt-find
                      matthew-bear))))

(define (plot-content #:inev? [inev? #f]
                      #:one? [one? inev?]
                      #:bears? [bears? one?]
                      #:arrow? [arrow? bears?]
                      #:incidental [incidental incidental]
                      #:inevitable [inevitable inevitable])
  (vc-append
   gap-size
   (hc-append gap-size (bigt "(") (scale racket-logo 0.5) (bigt ")"))
   (blank (* 3 gap-size))
   ((if arrow? values ghost) plot-arrow)
   (blank (* 3 gap-size))
   (ct-superimpose
    ((if bears? values ghost)
     (if one? one-bear all-bears))
    ((if inev? values ghost)
     (refocus (hc-append (- (* client-w 1/4))
                         one-bear
                         (vc-append (/ gap-size 2)
                                    (incidental)
                                    (t "vs.")
                                    (inevitable)))
              one-bear)))))

(define overall-incidental+inevitable
  (let ()
    (define g-inc (ghost (incidental)))
    (define g-inev (ghost (inevitable)))
    
    (define plot (plot-content #:inev? #t
                               #:incidental (lambda () g-inc)
                               #:inevitable (lambda () g-inev)))
    
    (define big-s 1.5)
    (define big-in (/ gap-size 2))
    (define inc-label
      (inset
       (scale (bt "Macros") big-s)
       big-in))
    (define inev-label
      (inset
       (scale (vl-append
               (current-line-sep)
               (bt "Multilanguage")
               (bt "programming"))
              big-s)
       big-in))
    (define inev-details-label
      (vl-append
       (current-line-sep)
       ;; scope, phase, declared language, transformats in intermediate, non-nested
       (para #:width (pict-width inev-label)
             #:fill? #f
             "Various ingredients related to scopes and phases")))
    
    (define (sim p q) (cc-superimpose p (launder (ghost q))))
  
    (lambda (n m-n ml-n #:details? [details? #f])
      (define inc
        (incidental
         (cellophane
          (fade-pict n (blank) (sim inc-label inev-label))
          m-n)))
      (define inev
        (inevitable
         (cc-superimpose
          ((if details? ghost values)
           (cellophane
            (fade-pict n (blank) (sim inev-label inc-label))
            ml-n))
          ((if details? values ghost)
           (fade-pict n (blank) inev-details-label)))))
      (define g2-inc (ghost (launder inc)))
      (define g2-inev (ghost (launder inev)))
      (let* ([p (cc-superimpose
                 (cellophane-pane plot n #:margin (* 3 gap-size))
                 (vc-append (* 2 gap-size) g2-inc g2-inev))]
             [p (slide-pict p inc g-inc g2-inc n)]
             [p (slide-pict p inev g-inev g2-inev n)])
        p))))

(define (plot-slides)
  (slide (plot-content))
  (slide (plot-content #:arrow? #t))
  (slide (plot-content #:bears? #t))
  (slide (plot-content #:one? #t))
  
  (play-n
   overall-incidental+inevitable))

(define (overall-incidental+inevitable-slides)
  (slide (overall-incidental+inevitable 1 1 1 #:details? #t))
  (slide (overall-incidental+inevitable 1 1 1 #:details? #f)))

(module+ main
  (plot-slides)
  (overall-incidental+inevitable-slides))
