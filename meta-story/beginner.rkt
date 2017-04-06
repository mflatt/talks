#lang slideshow
(require slideshow/balloon
         slideshow/code
         "config.rkt"
         "utils.rkt"
         "implementation.rkt"
         "contributor.rkt"
         "lesson.rkt")

(provide beginner-slides)

(define dr53 (bitmap "dr53.png"))
(define dr53/error (bitmap "dr53-err.png"))
(define dr53/config (bitmap "dr53-cfg.png"))

(define dr-title "DrScheme")

(define (dr-slides)
  (slide
   #:title dr-title
   #:layout 'tall
   'alts
   (let* ([student-lang
           (pin-balloon (wrap-balloon
                         (para "Language for students"
                               #:width (* 1/4 client-w)
                               #:fill? #f)
                         'nw 0 (- gap-size))
                        dr53
                        dr53 (shifted cc-find (* 2 gap-size) (* -1 gap-size)))]
          [gui-lang
           (pin-balloon (wrap-balloon
                         (para (it "Just right") "language for building the environment"
                               #:width (* 1/3 client-w)
                               #:fill? #f)
                         'nw 0 (- gap-size))
                        student-lang
                        dr53 (shifted lt-find (* (pict-width dr53) 3/5) gap-size))])
     (list
      (list (add-cite dr53
                      "Findler et al. [PLILP'97]; Felleisen et al. [SIGPLAN'98]"))
      (list student-lang)
      (list gui-lang)
      (list dr53/error)
      (list dr53/config)))))

;; ----------------------------------------

(define class-color "blue")
(define mixin-color "purple")

(define unit-color "black")
(define unit-bg-color "whitesmoke")

(define (ctt s) (scale (tt s) 0.75))

(define (enbox p w color
               #:superimpose [superimpose lt-superimpose]
               #:bg [bg "beige"])
  (let ([p (inset p 5)])
    (define (frame+bg p)
      (cc-superimpose (colorize (filled-rectangle (pict-width p) (pict-height p))
                                bg)
                      (frame p
                             #:line-width 2
                             #:color color)))
    (refocus (frame+bg (superimpose
                        p
                        (blank (+ w 10) 0)))
             p)))

(define (library name)
  (enbox (inset (if (string? name)
                    (bt name)
                    name)
                gap-size)
         0
         "black"
         #:bg "white"))

(define (make-class-like name methods color)
  (define n (scale (bt name) 0.8))
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

(define (make-mixin name . methods)
  (vc-append (colorize (linewidth 1 (vline 0 gap-size)) mixin-color)
             (make-class-like name methods mixin-color)))

(define (tree t l r)
  (define H gap-size)
  (let* ([p (vc-append (* 2 H)
                       t
                       (ht-append gap-size l r))]
         [p (pin-line p
                      t cb-find
                      t (shifted cb-find 0 H)
                      #:color class-color)]
         [p (pin-line p
                      l ct-find
                      l (shifted ct-find 0 (- H))
                      #:color class-color)]
         [p (pin-line p
                      r ct-find
                      r (shifted ct-find 0 (- H))
                      #:color class-color)]
         [p (pin-line p
                      r (shifted ct-find 0 (- H))
                      l (shifted ct-find 0 (- H))
                      #:color class-color)])
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
  (make-mixin "searching"
              "after-insert"
              "after-delete"))

(define autocomplete-mixin
  (make-mixin "autocomplete"
              "on-char"))

(define (make-unit imports body exports)
  (define i (apply hbl-append gap-size imports))
  (define e (apply hbl-append gap-size exports))
  (define w (max (pict-width i)
                 (pict-width e)
                 (pict-width body)))
  (vc-append -1
             (enbox i w unit-color #:bg unit-bg-color #:superimpose cc-superimpose)
             (enbox body w unit-color #:bg unit-bg-color)
             (enbox e w unit-color #:bg unit-bg-color #:superimpose cc-superimpose)))

(define evaluate-id (code evaluate))
(define find-help-id (code find-help))

(define (link-units id1 id2 u1 u2)
  (let* ([p (hb-append (* 2 gap-size) u1 u2)]
         [p (pin-arrow-line (/ gap-size 2)
                            p
                            (list u1 id1) rb-find
                            (list u2 id1) lt-find
                            #:start-angle (* -1/4 pi)
                            #:end-angle (* -1/4 pi)
                            #:color "orange"
                            #:line-width 3)]
         [p (pin-arrow-line (/ gap-size 2)
                            p
                            (list u2 id2) lb-find
                            (list u1 id2) rt-find
                            #:start-angle (* -3/4 pi)
                            #:end-angle (* -3/4 pi)
                            #:color "orange"
                            #:line-width 3)])
    p))

(define (abstractions-slides)
  (define (just-right s)
    (hbl-append (text "Just Right" `(italic . ,(current-main-font)) 40)
                (titlet " ")
                (titlet s)))
  
  (slide
   #:title (just-right "Abstractions: Mixins")
   (add-cite
    (ht-append
     (* 2 gap-size)
     (vc-append (* 2 gap-size)
                (tree editor-class
                      program-class
                      repl-class)
                (ht-append
                 gap-size
                 searching-mixin
                 autocomplete-mixin))
     (hc-append
      (* 2 gap-size)
      (colorize (arrow gap-size 0) "forestgreen")
      (tree editor-class
            (vc-append program-class
                       searching-mixin
                       autocomplete-mixin)
            (vc-append repl-class
                       searching-mixin
                       autocomplete-mixin))))
    "Flatt et al. [POPL'98]"))
  (slide
   #:title (just-right "Abstractions: Units")
   (add-cite
    (link-units
     evaluate-id
     find-help-id
     (make-unit (list find-help-id)
                (code
                 (define evaluate ....)
                 (define drscheme-window
                   .... (find-help) ....))
                (list evaluate-id))
     (make-unit (list evaluate-id)
                (code
                 (define find-help
                   .... (evaluate) ...))
                (list find-help-id)))
    "Flatt and Felleisen [PLDI'98]; Findler and Flatt [ICFP'98]")))

;; ----------------------------------------

(define (implementation-slides)
  (define (implementation-slide #:inevitable? [inevitable? #f]
                                #:incidental? [incidental? inevitable?]
                                #:defmacro? [defmacro? incidental?]
                                #:unit/sig? [unit/sig? (or defmacro? incidental?)]
                                #:m3+shriram? [m3+shriram? #f]
                                #:m3? [m3? (or m3+shriram? unit/sig?)]
                                #:class+unit!? [class+unit!? #f]
                                #:class+unit? [class+unit? (or class+unit!? m3?)])
    (define (add-notes q)
      (let* ([p (inset q 0 (* -2/3 (pict-height q)) 0 0)]
             [p (if incidental? 
                    (lt-superimpose
                     p
                     (inset (incidental "Using" (code defmacro))
                            (* -3.5 gap-size) 0 0 0))
                    p)]
             [p (if inevitable?
                    (rt-superimpose
                     p
                     (inset (inevitable "Using better tools for language variants")
                            0 0 (* -3.5 gap-size) 0))
                    p)])
        (refocus p q)))
    (define (maybe-add-cite add? cite p) (if add? (add-cite p cite) p))
    (slide
     #:title "DrScheme Implementation: The First Few Years"
     (maybe-add-cite
      m3+shriram?
      "Krishnamurthi et al. [GCSE'99]"
      (add-notes
       (make-implementation
        #:top (list
               (let ([p (hc-append gap-size
                                   (library "DrScheme")
                                   (library "Help")
                                   (t "..."))])
                 (if m3?
                     (vc-append gap-size
                                p
                                (let* ([m3 (scale (library "student languages") 0.75)]
                                       [m3 (if m3+shriram?
                                               (pin-balloon (wrap-balloon (format-bear shriram-bear
                                                                                       "Shriram"
                                                                                       "Krishnamurthi")
                                                                          'w (- gap-size) 0)
                                                            m3
                                                            m3 rc-find)
                                               m3)])
                                  (hc-append gap-size
                                             m3
                                             ((if unit/sig? values ghost)
                                              (let ([p (scale (library (tt "unit/sig")) 0.75)])
                                                (if defmacro?
                                                    (pin-balloon (wrap-balloon (code defmacro) 'nw (- gap-size) 0)
                                                                 p
                                                                 p (shifted rc-find (- gap-size) 0))
                                                    p))))))
                     p)))
        #:sep-show (if incidental? ghost values)
        #:bottom-show (if incidental? ghost values)
        #:bottom (list
                  (let* ([interp (hbl-append (bt "Scheme")
                                             (scale (t " interpreter") 0.75))]
                         [p (library (cond
                                      [class+unit?
                                       (vl-append (current-line-sep)
                                                  interp
                                                  (t "+ classes & mixins")
                                                  (t "+ units"))]
                                      [else interp]))])
                    (if class+unit!?
                        (pin-balloon (wrap-balloon (scale (bt "!") 1.5) 'e gap-size 0)
                                     p
                                     p (shifted lc-find gap-size 0))
                        p))
                  (library "GUI Widgets")))))))
  (implementation-slide)
  (implementation-slide #:class+unit? #t)
  (implementation-slide #:class+unit!? #t)
  (implementation-slide #:m3? #t)
  (implementation-slide #:m3+shriram? #t)
  (implementation-slide #:unit/sig? #t)
  (implementation-slide #:defmacro? #t)
  (implementation-slide #:incidental? #t)
  (implementation-slide #:inevitable? #t))

;; ----------------------------------------

(define (beginner-slides)
  (dr-slides)
  (abstractions-slides)
  (implementation-slides))

(module+ main
  (beginner-slides))
