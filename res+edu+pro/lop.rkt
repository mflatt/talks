#lang slideshow
(require racket/runtime-path
         slideshow/play
         pict/code
         "util.rkt"
         "gear.rkt"
         "logo.rkt"
         "person.rkt"
         "paper.rkt"
         "venn.rkt")

(provide lop-slides
         lop-main-logo-scale
         lop-main-gear-scale)

(define-runtime-path lambdahead-gif "logos/lambdahead.gif")
(define lambdahead (bitmap lambdahead-gif))

(define-runtime-path plt-scheme-png "logos/plt-scheme.png")
(define plt-scheme (bitmap plt-scheme-png))

(define (center-rotate p a #:around [around p] #:hide-rotate? [hide-rotate? #f])
  (cond
    [(zero? a) p]
    [else
     (define-values (cx cy) (cc-find p around))
     (pin-over (ghost p)
               cx cy
               (rotate (inset (if hide-rotate? (launder p) p) (- cx) (- cy) (- cx (pict-width p)) (- cy (pict-height p)))
                       a))]))

(define (turn-around p wrt-p n)
  (define-values (cx cy) (cc-find p wrt-p))
  (pin-over (ghost p)
            (* cx (- 1 n)) 0
            (scale p (min 1 (+ n 0.01)) 1)))

(define lop-main-logo-scale 1/3)
(define lop-main-gear-scale 4)

(define (lop-slides #:lang-gear-maker? [lang-gear-maker? #f]
                    #:gear [the-gear (gear)])
  (define main-gear (scale the-gear lop-main-gear-scale))
  (define faded-alpha 0.2)

  (define (add-gear #:show-main? [show-main? #t]
                    #:next? [next? #f]
                    #:spin [spin 0]
                    #:badge [badge (blank)]
                    #:next-badge [next-badge (blank)]
                    #:next-label [next-label #f]
                    #:next-scale [next-scale 1]
                    #:all-rotate [all-rotate 0]
                    #:alpha [alpha 1]
                    #:main-alpha [main-alpha 1]
                    #:arrow-alpha [arrow-alpha (min alpha main-alpha)]
                    #:collapse-n [collapse-n 0]
                    #:hide-rotate? [hide-rotate? #f])
    (define a (colorize (arrow (* 2 gap-size) 0)
                        "forestgreen"))
    (define sep (* 3 gap-size))
    (define normal-size
      (ghost (launder (hc-append sep main-gear a main-gear))))
    (define next-gear
      (scale ((if next? values ghost)
              (let* ([local-gear (launder main-gear)]
                     [g (cc-superimpose local-gear
                                        (center-rotate (if (zero? collapse-n)
                                                           next-badge
                                                           (blank))
                                                       (- all-rotate)))])
                (if next-label
                    (refocus (cc-superimpose g
                                             (center-rotate (refocus (hc-append gap-size
                                                                                (ghost local-gear)
                                                                                (scale
                                                                                 (hbl-append
                                                                                  (tt "#lang ")
                                                                                  (colorize
                                                                                   (tt next-label)
                                                                                   (current-id-color)))
                                                                                 (/ 1 next-scale)))
                                                                     local-gear)
                                                            (- all-rotate)))
                             g)
                    g)))
             next-scale))
    (define g-next-gear (if (positive? collapse-n)
                            (ghost next-gear)
                            next-gear))
    (define rotated-main-gear
      (cc-superimpose (if show-main?
                          (center-rotate main-gear spin #:hide-rotate? hide-rotate?)
                          (ghost main-gear))
                      (center-rotate badge (- all-rotate))))
    (define init-content
      (hc-append
       (* sep next-scale)
       (over-cellophane
        rotated-main-gear
        main-alpha)
       (over-cellophane (scale ((if (and next? (zero? collapse-n)) values ghost) a) next-scale)
                        arrow-alpha)
       (over-cellophane g-next-gear alpha)))
    (define content
      (if (positive? collapse-n)
          (let-values ([(tx ty) (lt-find init-content rotated-main-gear)]
                       [(fx fy) (lt-find init-content g-next-gear)])
            (pin-under init-content
                       (+ (* collapse-n tx) (* (- 1 collapse-n) fx))
                       (+ (* collapse-n ty) (* (- 1 collapse-n) fy))
                       (cellophane
                        (scale next-gear
                               (+ 1 (* collapse-n (sub1 (/ (pict-height main-gear) (pict-height next-gear))))))
                        (- 1 collapse-n))))
          init-content))
    (lc-superimpose
     normal-size
     (center-rotate content
                    all-rotate
                    #:around main-gear
                    #:hide-rotate? hide-rotate?)))

  (define moved-angle (* 1/8 pi))
  (define moved-angle2 (* 1/12 pi))
  (define moved-scale 0.6)

  (define students-icon (students))

  (define scaled-logo (scale logo lop-main-logo-scale))

  (define (main-spin n n2 n3 n4 [n5 0])
    (* 1/2 pi (+ (fast-middle n)
                 (fast-middle n2)
                 (fast-middle n3)
                 (- (fast-middle n4))
                 (fast-middle n5))))
  (define (all-rotate n n2)
    (+ (* moved-angle n)
       (* moved-angle2 n2)))

  (define (moved-students #:n n #:n2 [n2 0] #:n3 [n3 0] #:n4 [n4 0] #:n5 [n5 0]
                          #:labels? [labels? #f]
                          #:mode [mode 'all]
                          #:collapse-n [collapse-n 0]
                          #:main-logo [main-logo scaled-logo])
    (define a (all-rotate n n2))
    (add-gear #:next? #t
              #:badge main-logo
              #:next-badge (scale students-icon 1/2)
              #:next-scale (- 1 (* (- 1 moved-scale) n))
              #:spin (main-spin n n2 n3 n4 n5)
              #:all-rotate a
              #:next-label (and labels? "htdp/beginner")
              #:alpha (if (memq mode '(all edu+prod res+edu))
                          1
                          (if (memq mode '(lang))
                              0
                              faded-alpha))
              #:main-alpha (if (memq mode '(all res+edu lang)) 1 faded-alpha)
              #:collapse-n collapse-n))

  (define (moved-students+self #:n2 n2 #:n3 [n3 0] #:n4 [n4 0] #:n5 [n5 0]
                               #:labels? [labels? #f]
                               #:mode [mode 'all]
                               #:collapse-n [collapse-n 0]
                               #:main-logo [main-logo scaled-logo]
                               #:racket-logo [racket-logo #f])
    (lc-superimpose (add-gear #:show-main? #f
                              #:next? #t
                              #:next-badge (or racket-logo
                                               (scale logo 1/3))
                              #:next-scale moved-scale
                              #:all-rotate (+ (- moved-angle)
                                              (* n2 moved-angle)
                                              (if (eq? mode 'all) 0 (* n4 -1/8 pi)))
                              #:next-label (and labels? "racket")
                              #:alpha (if (memq mode '(all edu+prod res+prod lang)) 1 faded-alpha)
                              #:arrow-alpha (if (memq mode '(all lang)) 1 faded-alpha)
                              #:collapse-n collapse-n)
                    (moved-students #:n 1 #:n2 n2 #:n3 n3 #:n4 n4 #:n5 n5
                                    #:labels? labels?
                                    #:mode mode
                                    #:collapse-n collapse-n
                                    #:main-logo main-logo)))

  (define paper-icon (paper))

  (define (moved-students+self+paper #:n3 n3 #:n4 [n4 0] #:n5 [n5 0]
                                     #:labels? [labels? #f]
                                     #:mode [mode 'all]
                                     #:collapse-n [collapse-n 0]
                                     #:main-logo [main-logo scaled-logo]
                                     #:racket-logo [racket-logo #f])
    (lc-superimpose (add-gear #:show-main? #f
                              #:next? #t
                              #:next-badge (scale paper-icon 1.2)
                              #:next-scale moved-scale
                              #:all-rotate (- (+ moved-angle
                                                 moved-angle2))
                              #:next-label (and labels? "scribble/acmart")
                              #:alpha (if (memq mode '(all))
                                          1
                                          (if (eq? mode 'lang)
                                              0
                                              faded-alpha))
                              #:collapse-n collapse-n)
                    (moved-students+self #:n2 1 #:n3 n3 #:n4 n4 #:n5 n5
                                         #:labels? labels?
                                         #:mode mode
                                         #:collapse-n collapse-n
                                         #:main-logo main-logo
                                         #:racket-logo racket-logo)))


  (define (all #:labels? [labels? #f]
               #:mode [mode 'all]
               #:collapse-n [collapse-n 0]
               #:n4 [n4 0] #:n5 [n5 0]
               #:main-logo [main-logo scaled-logo]
               #:racket-logo [racket-logo #f])
    (lc-superimpose (add-gear #:show-main? #f
                              #:next? #t
                              #:next-badge (scale plt-scheme 1/2)
                              #:next-scale moved-scale
                              #:all-rotate (* 3/4 pi)
                              #:next-label (and labels? "scheme")
                              #:alpha (if (memq mode '(all res+prod lang)) 1 faded-alpha)
                              #:collapse-n collapse-n)
                    (add-gear #:show-main? #f
                              #:next? #t
                              #:next-badge lambdahead
                              #:next-scale moved-scale
                              #:all-rotate (* -3/4 pi)
                              #:next-label (and labels? "mzscheme")
                              #:alpha (if (memq mode '(all res+prod lang)) 1 faded-alpha)
                              #:collapse-n collapse-n)
                    (moved-students+self+paper #:n3 1 #:n4 n4 #:n5 n5
                                               #:labels? labels?
                                               #:mode mode
                                               #:collapse-n collapse-n
                                               #:main-logo main-logo
                                               #:racket-logo racket-logo)))

  (define (all+rhombus #:n4 [n4 0]
                       #:rhombus-logo rhombus-logo
                       #:main-logo main-logo)
    (lc-superimpose (add-gear #:show-main? #f
                              #:next? #t
                              #:next-badge rhombus-logo
                              #:next-scale moved-scale
                              #:all-rotate (* 1/8 pi))
                    (all #:mode 'lang #:n4 n4
                         #:main-logo main-logo)))

  (define (all+more #:n5 [n5 0]
                    #:more? [more? #f]
                    #:main-logo main-logo
                    #:racket-logo racket-logo)
    (define the-all
      (all #:mode 'all
           #:main-logo main-logo
           #:racket-logo racket-logo
           #:n4 1
           #:n5 n5))
    (cond
      [more?
       (define (more a)
         (add-gear #:show-main? #f
                   #:next? #t
                   #:next-scale moved-scale
                   #:all-rotate a))
       (lc-superimpose (more (* 0.47 pi))
                       (more pi)
                       (more (* -0.47 pi))
                       the-all)]
      [else the-all]))

  (define (add-venn the-all #:mode [mode 'all])
    (refocus (rt-superimpose
              (inset the-all 0 (* 3 gap-size) (* 5 gap-size) 0)
              (scale (make-venn #:icons? #f
                                #:res-alpha (if (eq? mode 'edu+prod) faded-alpha 1)
                                #:edu-alpha (if (eq? mode 'res+prod) faded-alpha 1)
                                #:prod-alpha (if (eq? mode 'res+edu) faded-alpha 1))
                     1/2))
             the-all))

  (cond
    [lang-gear-maker?
     (lambda (#:rhombus-logo [rhombus-logo #f]
              #:main-logo [main-logo scaled-logo]
              #:n4 [n4 0]
              #:just-main? [just-main? #f]
              #:finale? [finale? #f]
              #:collapsed? [collapsed? #f]
              #:n5 [n5 0])
       (cond
         [collapsed? (all #:main-logo main-logo
                          #:collapse-n 1)]
         [finale?
          (all+more #:main-logo main-logo
                    #:racket-logo rhombus-logo
                    #:n5 n5
                    #:more? (= n5 1))]
         [just-main?
          (add-gear #:badge main-logo
                    #:spin (main-spin 1 1 1 n4)
                    #:all-rotate (all-rotate 1 1)
                    #:hide-rotate? #t)]
         [rhombus-logo
          (all+rhombus #:n4 n4
                       #:rhombus-logo rhombus-logo
                       #:main-logo main-logo)]
         [else
          (all #:mode 'lang #:n4 n4
               #:main-logo main-logo)]))]
    [else
     (define name "Language-Oriented Programming")

     (play-n
      #:name name
      #:skip-last? #t
      #:steps 20
      (lambda (n)
        (add-gear #:spin (* (fast-middle n) (* pi)))))
     (slide #:name name (add-gear #:next? #t))

     (play-n
      #:name name
      #:skip-last? #t
      (lambda (n) (moved-students #:n n)))

     (play-n
      #:name name
      #:skip-last? #t
      (lambda (n2) (moved-students+self #:n2 n2)))

     (slide #:name name (moved-students+self+paper #:n3 0)) ; no labels

     (play-n
      #:name name
      #:skip-last? #t
      (lambda (n3) (moved-students+self+paper #:n3 n3 #:labels? #t)))

     (slide #:name name (all #:labels? #t))
     
     (define base-all (all))

     (slide #:name name
            (refocus (rt-superimpose
                      (inset base-all 0 0 (* 5 gap-size) 0)
                      (scale
                       (vc-append
                        (current-line-sep)
                        (bt "Language-Oriented")
                        (bt "Programming"))
                       1.2))
                     base-all))

     (define (add-venn-to-all #:mode [mode 'all])
       (add-venn (all #:mode mode) #:mode mode))

     (slide #:name name (add-venn-to-all))
     (slide #:name name (add-venn-to-all #:mode 'res+edu))
     (slide #:name name (add-venn-to-all #:mode 'edu+prod))
     (slide #:name name (add-venn-to-all #:mode 'res+prod))

     (cond
       [#t
        (play-n
         #:skip-last? #t ; leave last to "history-intro.rkt"
         #:name name
         (lambda (n)
           (all #:collapse-n (fast-middle n))))]
       [else
        (play-n
         #:name name
         #:skip-last? #t
         (lambda (n)
           (turn-around base-all main-gear (- 1 (fast-middle n)))))
        
        (define just-main (add-gear #:badge scaled-logo))
        (play-n
         #:name name
         #:skip-first? #t
         (lambda (n)
           (turn-around just-main main-gear (fast-middle n))))])
     
     (void)]))

(module+ main
  #;(slide ((lop-slides #:lang-gear-maker? #t)))
  (lop-slides))


