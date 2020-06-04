#lang slideshow
(require racket/runtime-path
         slideshow/code
         "util.rkt"
         "history.rkt"
         "unit.rkt")

(provide unit+mixin-slides)

(define ca-units+mixins (ca 1998 "Units and Mixins" #:who "Flatt et al."))
(define ca-mcmicmac (ca 2000 "Unit/lang" #:who "Krishnamurthi"))

(define class-color "blue")
(define mixin-color "purple")
(define mixin2-color "darkred")

(define class-line-color "darkgray")
(define mixin-line-color class-line-color)

(define line-width 4)

(define arrowhead-size 16)

(define-runtime-path greek1-png "greek1.png")
(define-runtime-path greek2-png "greek2.png")
(define-runtime-path greek3-png "greek3.png")
(define-runtime-path greek4-png "greek4.png")
(define greek (scale
               (hc-append
                (vc-append
                 (* 12 gap-size)
                 (bitmap greek3-png)
                 (bitmap greek2-png))
                (vl-append
                 (* 8 gap-size)
                 (bitmap greek4-png)
                 (bitmap greek1-png)))
               0.3))

(define (ctt s) (ghost (scale (tt s) 0.75)))

(define (enbox p w color
               #:superimpose [superimpose lt-superimpose]
               #:bg [bg "beige"])
  (let ([p (inset p 5)])
    (define (frame+bg p)
      (cc-superimpose (colorize (filled-rectangle (pict-width p) (pict-height p))
                                bg)
                      (frame p
                             #:line-width line-width
                             #:color color)))
    (refocus (frame+bg (superimpose
                        p
                        (blank (+ w 10) 0)))
             p)))

(define (make-class-like name methods color)
  (define n (ghost (scale (bt name) 0.8)))
  (define ms (colorize (apply vl-append
                              (current-line-sep)
                              (map ctt methods))
                       "darkgray"))
  (define w (max (pict-width n)
                 (pict-width ms)))
  (vc-append -0.5
             (panorama (enbox n w color #:superimpose ct-superimpose))
             (panorama (enbox ms w color))))

(define (make-class name . methods)
  (make-class-like name methods class-color))

(define (make-mixin mixin-color name . methods)
  (vc-append (colorize (linewidth line-width (vline 0 gap-size)) mixin-line-color)
             (make-class-like name methods mixin-color)))

(define vc*-append
  (case-lambda
    [(a b)
     (pin-under (vc-append a
                           (ghost b))
                b lt-find
                b)]
    [(a b . cs)
     (apply vc*-append (vc*-append a b) cs)]))

(define (tree t l r)
  (define H gap-size)
  (let* ([p (vc-append (* 2 H)
                       t
                       (ht-append gap-size l r))]
         [p (pin-line p
                      t cb-find
                      t (shifted cb-find 0 H)
                      #:under? #t
                      #:color class-line-color
                      #:line-width line-width)]
         [p (pin-line p
                      l ct-find
                      l (shifted ct-find 0 (- H))
                      #:under? #t
                      #:color class-line-color
                      #:line-width line-width)]
         [p (pin-line p
                      r ct-find
                      r (shifted ct-find 0 (- H))
                      #:under? #t
                      #:color class-line-color
                      #:line-width line-width)]
         [p (pin-line p
                      r (shifted ct-find 0 (- H))
                      l (shifted ct-find 0 (- H))
                      #:under? #t
                      #:color class-line-color
                      #:line-width line-width)])
    p))

(define editor-class 
  (make-class "editor"
              "..."))

(define program-class
  (make-class "program-editor"
              "insert"
              "delete"
              "..."))

(define repl-class
  (make-class "repl-editor"
              "on-char"
              "..."))

(define searching-mixin
  (make-mixin mixin-color
              "searching"
              "after-insert"
              "after-delete"))

(define autocomplete-mixin
  (make-mixin mixin2-color
              "autocomplete"
              "on-char"))

(define (unknown-parent p)
  (vc-append (let ([p (inset (rt "?") 0 4 0 0)])
               (cc-superimpose (colorize (filled-ellipse (pict-height p) (pict-height p))
                                         "darkgray")
                               (colorize p "white")))
             p))

(define (mixins-example)
  (define sep (* 4 gap-size))
  (ht-append
   sep
   (vc-append (* 2 gap-size)
              (tree editor-class
                    program-class
                    repl-class)
              (ht-append
               (* 3 gap-size)
               (unknown-parent searching-mixin)
               (unknown-parent autocomplete-mixin)))
   (inset
    (hc-append
     sep
     (colorize (arrow gap-size (* -1/8 pi)) "forestgreen")
     (tree editor-class
           (vc*-append program-class
                       searching-mixin
                       autocomplete-mixin)
           (vc*-append repl-class
                       searching-mixin
                       autocomplete-mixin)))
    0 100 0 0)))

(define (units-example #:cycle? [cycle? #f])
  (define-values (some-unit in out) (unit*))
  (define (fresh p) (inset p 0))
  (define a-unit (fresh some-unit))
  (define b-unit (fresh some-unit))
  (define c-unit (fresh some-unit))
  (define (link p from to side)
    (pin-arrow-line arrowhead-size
                    p
                    (list from out) (case side
                                      [(mid) cb-find]
                                      [(left) lc-find]
                                      [(right) rc-find])
                    (list to in) (case side
                                   [(mid) ct-find]
                                   [(left) lc-find]
                                   [(right) rc-find])
                    #:color link-color
                    #:line-width line-width
                    #:start-angle (case side
                                    [(mid) #f]
                                    [(left) (- pi)]
                                    [(right) 0])
                    #:end-angle (case side
                                  [(mid) #f]
                                  [(left) 0]
                                  [(right) (- pi)])))
  (let* ([p (vc-append 20
                       a-unit
                       b-unit
                       c-unit)]
         [p (link p a-unit b-unit 'mid)]
         [p (link p b-unit c-unit 'mid)]
         [p (link p a-unit c-unit 'left)]
         [p (if cycle?
                (link p c-unit a-unit 'right)
                p)])
    p))

(define (unit/lang-example)
  (define some-unit (unit))
  (define l-unit (let* ([p (inset some-unit 0 30)]
                        [p (pin-arrow-line arrowhead-size
                                           p
                                           p ct-find
                                           some-unit ct-find
                                           #:color link-color
                                           #:line-width line-width)]
                        [p (pin-arrow-line arrowhead-size
                                           p
                                           some-unit cb-find
                                           p cb-find
                                           #:color link-color
                                           #:line-width line-width)])
                   p))
  (hc-append
   (* 3 gap-size)
   (cc-superimpose l-unit (tt "Beginner"))
   (cc-superimpose l-unit (tt "Intermediate"))
   (cc-superimpose l-unit (tt "Advanced"))))

(define (unit+mixin-slides)
  (as-history
   #:edu 0

   (slide
    #:title ca-units+mixins
    #:name "Units and Mixins"
    (units-example))
   (slide
    #:title ca-units+mixins
    #:name "Units and Mixins"
    (units-example #:cycle? #t))

   (slide
    #:title ca-units+mixins
    #:name "Units and Mixins"
    (leftward (scale (mixins-example) 0.9))))

  (as-history
   #:edu 0
   #:prod 0
   (slide
    #:title ca-units+mixins
    #:name "Units and Mixins"
    (leftward greek)))

  (as-history
   #:res 2
   #:edu 0
   #:prod 0
   (slide
    #:title ca-units+mixins
    #:name "Units and Mixins"
    (leftward greek)))

  (as-history
   #:edu 0
   (slide
    #:title ca-mcmicmac
    #:name "Unit/lang"
    (leftward (unit/lang-example))))

  (as-history
   #:res 2
   #:edu 0
   (slide
    #:title ca-mcmicmac
    #:name "Unit/lang"
    (leftward (unit/lang-example)))))

(module+ main
  (unit+mixin-slides))

