#lang slideshow
(require slideshow/code
         racket/draw
         (for-syntax racket/base)
         "style.rkt"
         "in-file.rkt"
         "config.rkt")

(provide movie-ffi-slides
         movie-class-slides)

(define (lighten c)
  (define (light n) (- 255 (quotient (- 255 n) 2)))
  (make-color (light (send c red))
              (light (send c green))
              (light (send c blue))))

(define (enbox p #:color [color "darkblue"])
  (cc-superimpose
   (colorize
    (filled-rounded-rectangle (pict-width p) (pict-height p))
    color)
   p))

(define (enbox-layer label . desc)
  (lambda (col)
    (define p
      (vc-append
       (colorize (scale label 3/4) "white")
       (colorize (parameterize ([current-font-size (floor (* 3/4 (current-font-size)))])
                   (apply para #:width (/ client-w 3) #:fill? #f
                          desc))
                 (lighten (lighten col)))))
    (enbox (inset p (/ gap-size 2) (/ gap-size 2))
           #:color col)))

(define (layer label . desc)
  (define content
    ;; (blank (* 2 gap-size) (* 3/4 gap-size))
    (inset
     (cc-superimpose
      (blank (* client-w 1/4) 0)
      (parameterize ([current-font-size (floor (* 3/4 (current-font-size)))])
        (apply para #:width (/ client-w 3) #:fill? #f
               desc)))
     0 (* -1/3 gap-size)))
  (lambda (col)
    (mk-file #:name label
             #:color (lighten col)
             content)))

(define (select-arrow a)
  (case a
    [(->-1>)
     (hc-append (select-arrow '->)
                (select-arrow '-1>))]
    [else
     (define p (blank gap-size (* 0.8 gap-size)))
     (define r
       (pin-arrow-line (/ gap-size 2)
                       p
                       p ct-find
                       p cb-find
                       #:style (if (eq? a '-| |->)
                                   'dot
                                   'solid)
                       #:line-width 5
                       #:color (case a
                                 [(-> -| |->) runtime-color]
                                 [(-s>) runtime-only-color]
                                 [(-1>) comptime-color]
                                 [else "black"])))
     (scale r 2 1)]))

(define (enbox-double p col sc)
  (define s (/ gap-size 2))
  (lt-superimpose (inset (enbox (ghost p) #:color (lighten col))
                         0 s 0 0)
                  p))

(define (double p col sc) 
  (define s (/ gap-size 2))
  (lb-superimpose (scale ((layer (blank) (tt " ")) (lighten col)) sc)
                  (inset p 0 0 0 s)))

(define (add-desc a arrow)
  (refocus (hb-append a 
                      (t (case arrow
                           [(->) "uses values"]
                           [(-| |->) "dynamic use"]
                           [(-s>) "uses values & macros"]
                           [(-1>) "macros use values & macros"]
                           [(->-1>) "both"]
                           [else "???"])))
           a))

(define (add-arrow-key key-adj p arrow saw-arrows)
  (if (memq arrow saw-arrows)
      p
      (refocus (ct-superimpose p
                               (key-adj
                                (rt-superimpose
                                 (blank (/ client-w 2) 0)
                                 (let ([a (select-arrow arrow)])
                                   (inset (add-desc a arrow)
                                          0 (* (+ (* 4 (current-line-sep)) (pict-height a))
                                               (length saw-arrows))
                                          0 0)))))
               p)))
             
(define (evenize p)
  (define w (pict-width p))
  (define frac (- w (floor w)))
  (define i (floor (+ w frac)))
  (define e (if (even? i) i (add1 i)))
  (inset p (- e w) 0 0 0))

(define focus-scale 1.0) ; was 1.5

(define (stack #:scale [frame-scale 0.6]
               #:key-adj [key-adj values]
               #:skip-middle? [skip-middle? #f]
               #:skip-code? [skip-code? #f]
               . l)
  (define num (/ (sub1 (for/sum ([i (in-list l)]) (if (symbol? i) 0 1))) 2))
  (define (sc p) (scale p frame-scale))
  (define shift-amount (* client-w 1/3))
  (define (shift p) (inset p 0 0 shift-amount 0))
  (let loop ([a (blank)] [l l] [pos 0] [saw-arrows '(->-1>)] [prior #f])
    (define col (make-color (floor (* 170 (/ pos num)))
                            100
                            (floor (* 200 (/ (- num pos) num)))))
    (define (chain p c double arrow rest)
      (define v (sc (p col)))
      (define vx (scale (double v col frame-scale) focus-scale))
      (define base (vc-append a vx))
      (define (mk base hilite)
        (shift
         (let ([b base]
               [c (c hilite)])
           (refocus (if ((/ pos num) . < . 1/2)
                        (vc-append (/ (- client-h (pict-height b) (pict-height c)) 2)
                                   (shift b)
                                   (inset c 0 gap-size 0 0))
                        (hc-append (/ (- client-w (pict-width b) (pict-width c) (/ shift-amount 2)) 2)
                                   (ct-superimpose b (blank 0 client-h))
                                   (inset c 0 gap-size 0 0)))
                    b))))
      (define r (loop (add-arrow-key
                       key-adj
                       (cb-superimpose
                        (vc-append a
                                   (double v col frame-scale)
                                   (select-arrow arrow))
                        (ghost vx))
                       arrow
                       saw-arrows)
                      rest
                      (add1 pos)
                      (if (memq arrow saw-arrows)
                          saw-arrows
                          (cons arrow saw-arrows))
                      (lambda (p)
                        (mk (refocus (ct-superimpose p
                                                     (ghost base))
                                     base)
                            hilite))))
      (if skip-code?
          r
          (append
           (if prior
               (list (list (prior (vc-append a (scale v focus-scale)))))
               null)
           (if skip-middle?
               null
               (list (list (mk base values))
                     (list (mk base hilite))))
           r)))
    (match l
      [(list p)
       (define base (vc-append a (sc (p col))))
       (append
        (if skip-code?
            null
            (if prior
                (list (list (prior base)))
                null))
        (list (list (shift base))))]
      [(list* p c '* arrow rest)
       (chain p c double arrow rest)]
      [(list* p c arrow rest)
       (chain p c (lambda (p c s) p) arrow rest)])))

(define-syntax-rule (lcode s)
  (parameterize ([code-colorize-enabled #f])
    (code s)))

(define (link from to p)
  (pin-arrow-line (/ gap-size 2)
                  p
                  from ct-find 
                  to cb-find
                  #:color "blue"
                  #:line-width 3))

(define-syntax-rule (_code e)
  (parameterize ([code-italic-underscore-enabled #f])
    (code e)))

(define (smaller p) (scale p 0.8))

(define stx-use
  (lambda (hilite)
    (smaller
     (let-syntax ([define-syntax (make-code-transformer #'(code -define-syntax))])
       (code
        (define-syntax syntax-case**
          (lambda (x)
            (unless (#,(hilite (code stx-list?)) x)
              ....))))))))

(define kernel-use
  (lambda (hilite)
    (smaller
     (let-syntax ([define-values (make-code-transformer #'(hilite (code define-values)))]
                  [lambda (make-code-transformer #'(hilite (code lambda)))]
                  [if (make-code-transformer #'(hilite (code if)))]
                  [list? (make-code-transformer #'(hilite (code list?)))])
       (code
        (define-values (stx-list?)
          (lambda (p)
            (if (list? p)
                #t
                ....))))))))

(define objc-stack-l
  (list
   (layer
    (lcode ffi/unsafe/objc)
    "Objective-C interface")
   (lambda (hilite)
     (parameterize ([code-scripts-enabled #f])
       (let-syntax ([define-cstruct (make-code-transformer #'(hilite (code define-cstruct)))]
                    [_pointer (make-code-transformer #'(hilite (_code _pointer)))]
                    [_int (make-code-transformer #'(hilite (_code _int)))]
                    [_objc_ivar (make-code-transformer #'(_code _objc_ivar))])
         (code
          (define-cstruct _objc_ivar
            ([name _pointer]
             [ivar_type _pointer]
             [ivar_offset _int]))))))
   '-s>
   (layer
    (lcode ffi/unsafe)
    "foreign-library interface")
   (lambda (hilite)
     (smaller
      (let-syntax ([define (make-code-transformer #'(hilite (code define)))])
        (code
         (define (get-ffi-lib name 
                              [version/s ""]
                              #:fail [fail #f]
                              ....)
           ....)))))
   '-s>
   (layer
    (lcode racket/private/kw)
    "keyword arguments")
   (lambda (hilite)
     (smaller
      (let-syntax ([syntax-case (make-code-transformer #'(hilite (code syntax-case)))])
        (code
         (define-syntax (new-lambda stx)
           (syntax-case stx ()
             [(_ args body1 body ...)
              (if (simple-args? #'args)
                  ....)]))))))
   '* '-1>
   (layer
    (lcode racket/private/stxcase)
    "syntax pattern matching")
   stx-use
   '* '-1>
   (layer
    (lcode racket/private/stx)
    "simple syntax helpers")
   kernel-use
   '-s>
   (layer
    (lcode '#%kernel)
    "Racket core")))

(define racket/gui/base-code (code racket/gui/base))
(define frame%-code (code frame%))

(define movie-gui-stack-l
  (append
   (list
    (layer
     (lcode "movie.rkt")
     "an application")
    (lambda (hi)
      ((if (eq? hi hilite)
           link
           (lambda (a b c) c))
       frame%-code
       racket/gui/base-code
       (code
        #,(tt "#lang") racket/base
        (require #,racket/gui/base-code
                 racket/class
                 "movie-panel.rkt")
        code:blank
        (define f (new #,(hi frame%-code)
                       [label "Stay Functional"]
                       [width 800]
                       [height 600])))))
    '->
    (layer
     (lcode racket/gui/base)
     "portable GUI classes")
    (lambda (hilite)
      (code
       (define frame%
         (class ....
           (make-object #,(hilite (code wx-frame%)) ....) ...))))
    '* '->
    (layer
     (lcode mred/private/wx/platform)
     "platform-specific GUI classes")
    (lambda (hilite)
      (code 
       (define-runtime-module-path-index platform-lib
         ....
         '(lib #,(hilite (code "mred/private/wx/cocoa/platform.rkt")))
         ....)))
    '-| |->
    (layer
     (lcode mred/private/wx/cocoa/platform)
     "Mac-specific GUI classes")
    (lambda (hilite)
      (let-syntax ([define-objc-class (make-code-transformer #'(hilite (code define-objc-class)))]
                   [_BOOL  (make-code-transformer #'(hilite (_code _BOOL)))]
                   [_void  (make-code-transformer #'(_code _void))]
                   [_id (make-code-transformer #'(hilite (_code _id)))])
        (code
         (define-objc-class InputMethodPanel NSPanel
           []
           [- _BOOL (canBecomeKeyWindow) #f]
           [- _BOOL (canBecomeMainWindow) #f]
           [- _void (windowDidResize: [_id notification])
              (reset-input-method-window-size)]))))
    '* '-s>)
   objc-stack-l))

(define movie-panel.rkt-code (code "movie-panel.rkt"))
(define movie-panel%-code (code movie-panel%))

(define movie-ffi-stack-l
  (append
   (list
    (layer
     (lcode "movie.rkt")
     "an application")
    (lambda (hi)
      ((if (eq? hi hilite)
           link
           (lambda (a b c) c))
       movie-panel%-code
       movie-panel.rkt-code
       (code
        #,(tt "#lang") racket/base
        (require racket/gui/base
                 racket/class
                 #,movie-panel.rkt-code)
        code:blank
        ....
        (define mp (new #,(hi movie-panel%-code)
                        [parent f]
                        [file "stay-functional.mp4"])))))
    '->
    (layer
     (lcode "movie-panel.rkt")
     "Movie player bindings")
    (lambda (hilite)
      (let-syntax ([define-objc-class (make-code-transformer #'(hilite (code define-objc-class)))]
                   [tell (make-code-transformer #'(hilite (code tell)))]
                   [super-tell (make-code-transformer #'(hilite (code super-tell)))]
                   [self (make-code-transformer #'(hilite (code self)))]
                   [_void  (make-code-transformer #'(_code _void))])
        (code
         (define-objc-class MyMovieView QTMovieView
           []
           [-a _void (keyDown: evt)
               (when (equal? "a" (event-string evt))
                 (tell self gotoBeginning: self))
               (super-tell keyDown: evt)]))))
    '-s>)
   objc-stack-l))

(define real-slice-title "Dependency Chain in a Racket Program")

(define (movie-ffi-slides)
  (slide
   #:title real-slice-title
   #:layout 'tall
   'alts
   (let ([l (apply stack #:scale 0.75 movie-ffi-stack-l)])
     (if gpce-version?
         (list (last l))
         l))))

(define c... (lambda (hilite) (blank)))

(define alt-movie-stack
 (stack
  #:scale 0.75
  #:skip-code? #t
  (layer
   (lcode "movie.rkt")
   "an application")
  (lambda (hilite)
    (let-syntax ([new (make-code-transformer #'(hilite (code new)))])
      (code
       (new frame%
            [label "Stay Functional"]
            [width 800]
            [height 600]))))
  '-s>
  (layer
   (lcode racket/class)
   "syntax for classes")
  (lambda (hilite)
    (let-syntax ([syntax-parse (make-code-transformer #'(hilite (code syntax-parse)))])
      (code
       (define-syntax (new stx)
         (syntax-parse stx
           ....)))))
  '* '-1>
  (layer
   (lcode syntax/parse)
   "syntax patterns")
  (lambda (hilite)
    (let-syntax ([-> (make-code-transformer #'(hilite (code ->)))]
                 [any/c (make-code-transformer #'(hilite (code any/c)))])
      (code
       (-> any/c any/c))))
  '* '-s>
  (layer
   (lcode racket/contract)
   "contract system")
  (lambda (hilite)
    (let-syntax ([syntax-case (make-code-transformer #'(hilite (code syntax-case)))])
      (code
       (define-syntax (-> stx) 
         (syntax-case stx ....)))))
  '* '-1>
  (layer
   (lcode racket/base)
   "normal Racket")
  stx-use
  '* '->-1>
  (layer
   (lcode racket/private/stx)
   "simple syntax helpers")
  kernel-use
  '-s>
  (layer
   (lcode '#%kernel)
   "Racket core")))

(define (movie-class-slides)
  (slide
   #:title (string-append "Another " real-slice-title)
   'alts
   alt-movie-stack))

(module+ main
  (movie-ffi-slides)
  (movie-class-slides))
