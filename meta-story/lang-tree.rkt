#lang slideshow
(require (for-syntax racket/base)
         racket/runtime-path
         slideshow/play
         lang-slide
         racket/draw
         racket/class
         "utils.rkt")

(provide lang-tree-slides
         
         make-racket-dots
         make-racket-tree)

(define-syntax-rule (define-icons name ...)
  (begin
    (define-icon name) ...))

(define-syntax (define-icon stx)
  (syntax-case stx ()
    [(_ id)
     #`(begin
         (define-runtime-path icon-path #,(format "language-logos/~a-logo.png" (syntax-e #'id)))
         (define id (lang-icon-pict icon-path)))]))

(define (lang-icon-pict path)
  (define S 96)
  (define p (bitmap path))
  (define s (min (/ S (pict-width p))
                 (/ S (pict-height p))))
  (cc-superimpose (pict->pre-render-pict (scale p s)) (blank S)))

(define-icons racket scala c++ haskell ocaml clojure java python smalltalk
  linux windows macos)

(define (connect orig-p from to #:center? [center? #f])
  (define-values (fx fy) (ct-find orig-p from))
  (define-values (tx ty) (cb-find orig-p to))
  (define midpt (lambda (p q)
                  (values fx (/ (+ fy ty) 2))))
  (let* ([p (ghost orig-p)]
         [p (pin-line p
                      from (if center? cc-find ct-find)
                      from midpt
                      #:color "gray")]
         [p (pin-line p
                      from midpt
                      to (if center? cc-find cb-find)
                      #:color "gray")])
    (refocus (cc-superimpose (launder p)
                             orig-p)
             orig-p)))

(define (connect* do? p from tos #:center? [center? #f])
  (if do?
      (for/fold ([p p]) ([to (in-list tos)])
        (connect p from to #:center? center?))
      p))

(define (connect** do? p froms tos)
  (if do?
      (for/fold ([p p]) ([from (in-list froms)])
        (connect* #t p from tos))
      p))

(define (dot color)
  (colorize (filled-ellipse 24 24) color))


(define v-sep 80)

(define (dots p #:dot [dot dot])
  (let ([dot1 (dot "lightblue")]
        [dot2 (dot "lightgreen")]
        [dot3 (dot "pink")])
    (refocus
     (connect*
      #:center? #t
      #t
      (vc-append (/ v-sep 2)
                 (hc-append 10 dot1 dot2 dot3)
                 p)
      p
      (list dot1 dot2 dot3))
     p)))

(define (make-racket-dots)
  (scale (dots racket) 2))

(define (make-racket-tree #:racket [racket racket]
                          #:down [down (pict-height racket)])
  (inset (scale (dots racket
                      #:dot (lambda (color)
                              (define c (dot color))
                              (refocus
                               (cc-superimpose (ghost c)
                                               (launder
                                                (rotate (dots c
                                                              #:dot (lambda (color2)
                                                                      (dot
                                                                       (darker-color
                                                                        (send the-color-database find-color color)
                                                                        (cond
                                                                         [(equal? color2 "lightblue") 0.8]
                                                                         [(equal? color2 "pink") 0.4]
                                                                         [else 0.6])))))
                                                        (cond
                                                         [(equal? color "lightblue") (* 1/3 pi)]
                                                         [(equal? color "pink") (* -1/3 pi)]
                                                         [else 0]))))
                               c)))
                2)
         0 down 0 0))

(define (lang-tree-slides)
  (define tree-title "Tree of Languages")
  
  (define (tree-slide #:lib? [lib? #f]
                      #:os? [os? lib?]
                      #:jvm? [jvm? os?]
                      #:racket-show [racket-show values])
    (connect**
     os?
     (connect*
      jvm?
      (let* ([sep 40]
             [dots (lambda (p)
                     (if lib?
                         (dots p)
                         p))]
             [java (refocus (vc-append v-sep
                                       ((if jvm? values ghost)
                                        (hc-append sep (dots scala) (dots clojure)))
                                       java)
                            java)])
        (vc-append
         v-sep
         (ghost (launder racket))
         (hc-append sep (dots c++) (dots python) (dots ocaml) (dots haskell)
                    (dots java) (dots smalltalk) (racket-show (dots racket)))
         ((if os? values ghost)
          (hc-append sep linux (scale windows 0.8) macos))))
      java
      (list scala clojure))
     (list linux windows macos)
     (list c++ python ocaml haskell java smalltalk racket)))
  
  (slide #:name tree-title (tree-slide))
  (slide #:name tree-title (tree-slide #:jvm? #t))
  (slide #:name tree-title (tree-slide #:os? #t))
  (slide #:name tree-title (tree-slide #:lib? #t))
  
  (define g-racket (ghost (launder (scale racket 2))))
  (define down (pict-height g-racket))
  (play-n
   #:skip-first? #t
   #:name tree-title
   (lambda (n)
     (slide-pict (cc-superimpose (cellophane (tree-slide #:racket-show ghost) (- 1 n))
                                 (inset g-racket 0 down 0 0))
                 (scale (dots racket) (+ 1 n))
                 racket
                 g-racket
                 n)))
  (define g2-racket (ghost (launder racket)))
  (define racket-tree (make-racket-tree #:racket g2-racket #:down down))
  (define small-size 0.5)
  (define g-small-racket (ghost (launder (scale racket small-size))))
  (define small-racket-pane
    (pin-over full-page
              205 420
              g-small-racket))
  (play-n
   #:skip-last? #t
   (lambda (n)
     (slide-pict (cc-superimpose (cellophane racket-tree (- 1 n))
                                 small-racket-pane)
                 (scale racket (+ small-size (* (- 2 small-size) (- 1 n))))
                 g2-racket 
                 g-small-racket
                 n)))
  
  (slide (cc-superimpose (inset (pict->pre-render-pict (langs-pict #t)) (- margin))
                         (pin-over small-racket-pane
                                   g-small-racket lt-find
                                   (scale racket small-size)))))

(module+ main
  (lang-tree-slides))
