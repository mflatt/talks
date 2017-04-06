#lang slideshow
(require "castle.ss"
         "movie.ss"
         slideshow/play
         slideshow/balloon
         slideshow/code
         slideshow/flash
         scheme/class
         scheme/gui/base
         racket/runtime-path)

(provide analogy-slides
         analogy-again-slides
         book-icon
         drscheme-screen
         plt-langs)

(define (book p)
  (let ([w (pict-width p)])
    (let ([page (cc-superimpose (inset p 5 0 5 20) (blank 0 (* w 1.5)))]
          [frame (lambda (p)
                   (frame (cc-superimpose
                           (colorize (filled-rectangle (pict-width p) (pict-height p)) "white")
                           p)))])
      (let ([page (frame page)])
        (let loop ([n 6])
          (if (zero? n)
              page
              (ct-superimpose
               page
               (inset (loop (sub1 n)) 6 6 0 0))))))))

(define (brt s)
  (text s `(bold . roman) (current-font-size)))

(define equals (scale (t "=") 3))

(define lang-for-docs
  (vc-append
   (current-line-sep)
   (t "language")
   (t "for")
   (t "documents")))

(define tool-for-docs
  (vc-append
   (current-line-sep)
   (t "tool")
   (t "for")
   (t "documents")))

(define doc-framework
  (vc-append
   (current-line-sep)
   (t "language")
   (t "for")
   (t "languages")))

(define many-tools
  (cc-superimpose
   (vc-append
    (current-line-sep)
    (t "desktop")
    (t "of")
    (t "tools"))
   (ghost doc-framework)))

(define (hash-lang p)
  (code #,(tt "#lang") #,p))

(define plt-langs
  (cb-superimpose (scale plt-bm 0.5)
                  (scale
                   (inset
                    (vc-append
                     (/ gap-size 2)
                     (hc-append (* gap-size 2)
                                (hash-lang (code racket))
                                (hash-lang (code scribble/base)))
                     (inset (hash-lang (code scribble/manual)) 0 0 gap-size 0)
                     (inset
                      (hash-lang (code scribble/lp))
                      (* 4 gap-size) 0 0 0)
                     (inset (hash-lang (code at-exp racket)) 0 0 (* 5 gap-size) 0))
                    0 0 0 (- (* 4 gap-size)))
                   0.5)))

(define book-icon
  (book (scale
         (vc-append
          (current-line-sep)
          (brt "How to")
          (brt "use")
          (brt "Racket"))
         0.75)))

(define-runtime-path drscheme-png "drracket.png")
(define-runtime-path word-png "word.png")
(define-runtime-path apps-png "apps.png")

(define drscheme-screen
  (scale (bitmap drscheme-png) 0.5))
(define word-screen
  (scale (bitmap word-png) 0.5))

(define apps-screen
  (let ([bm (bitmap apps-png)])
    (refocus (cc-superimpose
              (scale bm (* 2 (min (/ (pict-width plt-langs)
                                     (pict-width bm))
                                  (/ (pict-height plt-langs)
                                     (pict-height bm)))))
              (ghost plt-langs))
             plt-langs)))

(define (analogy-slide programmer? skip-balloons?
                       a= b b= blbl? c c= clbl? overlay)
  (let* ([p (overlay
             (table
              4
              (list (scale castle 0.5)
                    (a= equals)
                    (a= book-icon)
                    (a= (t "document"))
                    (b (scale million-well 0.5))
                    (b= equals)
                    (b= (if programmer?
                            drscheme-screen
                            word-screen))
                    (b= (if programmer?
                            lang-for-docs
                            tool-for-docs))
                    (c (scale kingdom 0.5))
                    (c= equals)
                    (c= (if programmer?
                            plt-langs
                            apps-screen))
                    (c= (if programmer?
                            doc-framework
                            many-tools)))
              cc-superimpose cc-superimpose
              (* 2 gap-size) (* 1.5 gap-size)))]
         [p (if (and (not skip-balloons?) blbl?)
                (with-scribble p)
                p)]
         [p (if (and (not skip-balloons?) clbl?)
                (with-plt p)
                p)])
    p))

(define (analogy-slide-all overlay)
  (analogy-slide #t #f values values values #f values values #f overlay))

(define highlighted
 (lambda (n)
   (analogy-slide #t #f values values values #f values values #f
                  (lambda (p)
                    (let-values ([(x_ y0) (lt-find p million-well)]
                                 [(x0 y_) (lt-find p kingdom)]
                                 [(x1 y1) (rb-find p kingdom)]
                                 [(w) (pict-width p)]
                                 [(h) (pict-height p)])
                      (cc-superimpose
                       p
                       (cellophane
                        (dc (lambda (dc x y)
                              (let ([c (send dc get-clipping-region)]
                                    [r1 (make-object region% dc)]
                                    [r2 (make-object region% dc)]
                                    [b (send dc get-brush)])
                                (send r1 set-rounded-rectangle
                                      (+ x (- x0 (* gap-size .5)))
                                      (+ y (- y0 (* gap-size .5)))
                                      (+ (- x1 x0) (* 1 gap-size))
                                      (+ (- y1 y0) (* 1 gap-size))
                                      10)
                                (send r2 set-rectangle 0 0 1024 768)
                                (send r2 subtract r1)
                                (send dc set-clipping-region r2)
                                (send dc set-brush "black" 'solid)
                                (send dc draw-rectangle 0 0 1024 768)
                                (send dc set-clipping-region c)
                                (send dc set-brush b)))
                            w h)
                        (* n 0.25))))))))

(define (offset find dx dy)
  (lambda (p p2)
    (let-values ([(x y) (find p p2)])
      (values (+ x dx) (+ y dy)))))

(define (x-wrap-balloon p d dx dy)
  (wrap-balloon p d dx dy balloon-color 10))

(define (analogy-slides #:programmer? [programmer? #t]
                        #:skip-balloons? [skip-balloons? #f])
  (define lbl? (and skip-balloons? values))
  (let loop ([l (list ghost ghost ghost lbl? ghost ghost lbl?)]
             [pre null])
    (unless (and (pair? l)
                 (eq? (car l) values))
      (if (and condense?
               (pair? l))
          (skip-slides 1)
          (slide #:name "Analogy" (apply analogy-slide 
                                         programmer? 
                                         skip-balloons? 
                                         (append pre l (list values))))))
    (unless (null? l)
      (loop (cdr l) (cons values pre)))))
    

(define (analogy-again-slides)
  (play-n #:name "Analogy" #:steps 5 highlighted)

  (let* ([h (highlighted 1.0)]
         [compare (lambda (c a b)
                    (inset (para #:fill? #f c (bt a) (scale (t b) 0.8)) 5))]
         [h (pin-balloon (x-wrap-balloon
                          (compare "both" "princess-complete" "in theory")
                          'sw (- gap-size) gap-size)
                         h
                         million-well
                         (offset rb-find (* 3 gap-size) 0))])
    (slide h)
    (let ([h (pin-balloon (x-wrap-balloon
                           (compare "this one" "better" "in practice")
                           'w (- gap-size) 0)
                          h
                          kingdom
                          (offset rc-find (- (* 2 gap-size)) 0))])
      (slide h)))

  (play-n #:name "Analogy" #:steps 5 #:skip-first? #t (lambda (n) (highlighted (- 1 n))))

  (let* ([h (with-scribble (highlighted 0.0))])
    (slide #:name "Analogy" h)
    (slide #:name "Analogy" (with-plt h))))

(define (with-X h c target)
  (let* ([s (lambda (c)
              (x-wrap-balloon
               (inset c 5)
               'se gap-size gap-size))]
         [h (pin-balloon (s c)
                         h
                         target
                         lt-find)])
    h))

(define (with-scribble p)
  (with-X p (bt "Scribble") lang-for-docs))
(define (with-plt p)
  (with-X p (bt "Racket") doc-framework))

(module+ main
  (analogy-slides))
