#lang slideshow
(require slideshow/code
         racket/draw
         racket/class
         slideshow/play
         "../scribble/talk/movie.ss"
         lang-slide
         "../scribble/talk/castle.ss"
         "features.rkt")

(provide racket-way-slides 
         racket-is-slides 
         all-langs)

(define all-langs (langs-pict #t #:fit? #t))

(define (do-racket-way-slides slide
                              #:plt-bm [plt-bm plt-bm]
                              #:castle [castle castle]
                              #:million-well [million-well million-well]
                              #:kingdom [kingdom kingdom])
  (define (pitem p . rest)
    (hc-append gap-size
               p
               (apply para rest)))
  
  (let ([gap (blank 0 gap-size)])
    (slide
     #:title (hc-append gap-size
                        (scale plt-bm 0.33)
                        (titlet "The Racket Way"))
     (pitem (scale castle 0.25)
            "Everything is a program")
     'next
     gap
     (pitem (scale million-well 0.25)
            "Concepts are programming language constructs")
     'next
     gap
     (pitem (scale kingdom 0.25)
            "The programming language is extensible"))))

(define (racket-way-slides)
  (do-racket-way-slides slide))

(define (pseudo-slide #:title t . orig-l)
  (define (evenize p)
    (let ([w (pict-width p)])
      ;; Force even size:
      (inset p 0 0 (+ (- (ceiling w) w)
                      (modulo (ceiling w) 2)) 0)))
  (define content
    (apply vc-append 
           gap-size 
           (map
            evenize
            (filter (lambda (a) (not (eq? a 'next))) orig-l))))
  (ct-superimpose (vc-append (evenize (if (string? t) (titlet t) t))
                             (cc-superimpose titleless-page 
                                             content))
                  full-page))

(define (racket-is-slides)
  (define (pt p) (ghost (launder p)))

  (define plt-bm-src (pt plt-bm))
  (define plt-bm-dest (pt plt-bm))
  (define castle-src (pt castle))
  (define castle-dest (pt castle))
  (define million-well-src (pt million-well))
  (define million-well-dest (pt million-well))
  (define kingdom-src (pt kingdom))
  (define kingdom-dest (pt kingdom))

  (define ?-plt-bm
    (cc-superimpose plt-bm-src
                    (let ([s (pict-width plt-bm)])
                      (dc (lambda (dc x y)
                            (define b (send dc get-brush))
                            (define p (send dc get-pen))
                            (send dc set-pen (make-pen #:style 'transparent))
                            (send dc set-brush
                                  (make-brush #:gradient
                                              (make-object radial-gradient%
                                                           (+ x (/ s 2)) (+ y (/ s 2))
                                                           (/ s 6)
                                                           (+ x (/ s 2)) (+ y (/ s 2))
                                                           (/ s 2)
                                                           (list (list 0 (make-color 0 0 0 0.5))
                                                                 (list 1 (make-color 0 0 0 0.0))))))
                            (send dc draw-ellipse x y s s)
                            (send dc set-pen p)
                            (send dc set-brush b))
                          s s))
                    (scale (colorize (bt "?") "white") 8)))

  (define (nonterm s) (colorize (t (format "\u2329~a\u232A" s)) "black"))
  
  (slide
   #:title "The Core Racket Grammar?"
   (table 3
          (list (nonterm "module")
                (tt "::=")
                (code #,(tt "#lang") #,(nonterm "module-name") #,(nonterm "any")))
          cc-superimpose cc-superimpose
          gap-size gap-size)
   'next
   (blank (* 2 gap-size))
   (colorize
    (para #:align 'right
          "... plus a mapping from" 
          (hbl-append (nonterm "module-name") (t "s"))
          "to" (hbl-append (nonterm "module") (t "s")))
    "blue")
   'next
   (blank)
   (colorize
    (para #:align 'right "... plus one pre-defined" (nonterm "module-name"))
    "blue"))

  (define narrow (* client-w 1/2))

  (define racket-way-template
    (do-racket-way-slides pseudo-slide
                          #:plt-bm plt-bm-dest
                          #:castle castle-dest
                          #:million-well million-well-dest
                          #:kingdom kingdom-dest))

  (for ([i 6])
    (define (pl >=) (if (i . >= . 1) values ghost))
    (define (fam >=) (if (i . >= . 2) values ghost))
    (define (tool >=) (if (i . >= . 3) values ghost))
    (define (way >=) (if (i . >= . 4) values ghost))
    (define (end >=) (if (i . >= . 5) values ghost))
    (define (make n)
      (define base
        (cc-superimpose
         ((lambda (p) (cellophane p (max 0 (- 1 (* 2 n)))))
          (pseudo-slide
           #:title "Racket is..."
           (blank (* 2 gap-size))
           (hc-append (* 3 gap-size)
                      (scale ((way <) ?-plt-bm)
                             0.75)
                      (scale
                       (vl-append
                        gap-size
                        ((pl >=)
                         (item #:width narrow
                               "... a programming language"))
                        ((fam >=)
                         (item #:width narrow
                               "... a family of programming languages"))
                        ((tool >=)
                         (item #:width narrow
                               "... a set of programming tools"))
                        ((way >=)
                         (item #:width narrow
                               "... a" (it "way") "of programming")))
                       1.0))
           (cc-superimpose
            ((pl =) example-racket-program)
            ((fam =) (scale all-langs 1/2))
            ((tool =) ide-screenshot)
            ((way >=) (hc-append (* 3 gap-size)
                                 (scale castle-src 0.5)
                                 (scale million-well-src 0.5)
                                 (scale kingdom-src 0.5))))))
         ((lambda (p) (cellophane p (max 0.0 (* 4 (- n 0.75)))))
          (cb-superimpose racket-way-template
                          (tt "racket-lang.org")))))
      (define (scale-slide-pict base p src dest n)
        (define-values (sx sy) (lt-find base src))
        (define-values (sx2 sy2) (rb-find base src))
        (define-values (dx dy) (lt-find base dest))
        (define-values (dx2 dy2) (rb-find base dest))
        (define s (+ (* (- 1 n) (/ (- sx2 sx) (pict-width p)))
                     (* n (/ (- dx2 dx) (pict-width p)))))
        (slide-pict base (scale p s) src dest n))
      (let ([p (lt-superimpose
                (scale-slide-pict (ghost base) plt-bm plt-bm-src plt-bm-dest n)
                base)])
        (if (i . >= . 4)
            (let* ([p (scale-slide-pict p castle castle-src castle-dest n)]
                   [p (scale-slide-pict p million-well million-well-src million-well-dest n)]
                   [p (scale-slide-pict p kingdom kingdom-src kingdom-dest n)])
              p)
            p)))
    (when (= i 5)
      (for ([j 10])
        (slide #:timeout 0.05 (make (/ j 10.0)))))
    (if (and condense? (i . < . 4))
        (skip-slides 1)
        (slide (make (if (= i 5) 1.0 0.0)))))

  (void))

(module+ main
  (racket-way-slides)
  (racket-is-slides))
