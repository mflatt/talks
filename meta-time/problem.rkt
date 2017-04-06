#lang slideshow
(require slideshow/code
         slideshow/face
         slideshow/balloon
         racket/draw
         "lightbulb.rkt"
         "style.rkt"
         "in-file.rkt")

(provide use-slides
         eval-when-slides
         phase-slides)

(define-syntax-rule (scode e ...)
  (scale (code e ...) 0.8))

(define a-size (/ gap-size 2))
(define l-width 3)

(define lispy-suffix "lsp")

(define (nt s) (colorize (t s) "blue"))

(define (note icon
              #:-append [-append hc-append]
              #:width [width (* client-w 1/4)]
              . content)
  (balloon-pict
   (wrap-balloon
    (let ([p (apply para
                    #:width width
                    #:fill? #f
                    content)])
      (if icon
          (-append
           gap-size
           icon
           p)
          p))
    's 0 0)))

(define (link p from to
              #:from [from-find cb-find]
              #:to [to-find c*t-find]
              #:color [color runtime-color]
              #:alpha [alpha 1.0]
              #:hilite? [hilite? #f]
              #:start-angle [start-angle #f]
              #:end-angle [end-angle #f])
  (let ([p (if hilite?
               (pin-arrow-line (+ a-size 4)
                               p
                               from from-find 
                               to (adj to-find -2 2)
                               #:color "pink"
                               #:line-width 7)
               p)])
    (pin-arrow-line a-size
                    p
                    from from-find 
                    to to-find
                    #:color color
                    #:alpha alpha
                    #:line-width l-width
                    #:start-angle start-angle
                    #:end-angle end-angle)))

(define (llink p from to
               #:from [from-find rc-find]
               #:to [to-find c*t-find]
               #:alpha [alpha 1.0])
  (link p from to
        #:from from-find
        #:to to-find
        #:alpha alpha
        #:color load-color))

(define (lists-slide #:hilite-use? [hilite-use? #f]
                     #:hilite-bind? [hilite-bind? #f]
                     #:show-err? [show-err? #f]
                     #:show-loads? [show-loads? #t]
                     #:show-deps? [show-deps? #t]
                     #:bad-order? [bad-order? #f]
                     #:show-rev? [show-rev? #f]
                     #:hilite-bad? [hilite-bad? #f]
                     #:add-load? [add-load? #f]
                     #:use? [use? #f]
                     #:use-note? [use-note? use?]
                     #:show-flat? [show-flat? (not use?)])

  (define (sel-use l u)
    (lbl-superimpose 
     ((if use? ghost values) l)
     ((if use? values ghost) u)))

  (define load-grocery (sel-use
                        (code (load "grocery.scm"))
                        (code (use "grocery.lsp"))))
  (define load-list-load (code (load "list.scm")))
  (define load-list-use (code (use "list.lsp")))
  (define load-list (sel-use load-list-load
                             load-list-use))
  (define load-top-10-load (code (load "top-10.scm")))
  (define load-top-10-use (code (use "top-10.lsp")))
  (define load-top-10 (sel-use load-top-10-load
                               load-top-10-use))
  (define load-list-too-load (code (load "list.scm")))
  (define load-list-too-use (code (use "list.lsp")))
  (define load-list-too (sel-use load-list-too-load
                                 load-list-too-use))

  (define fold-use ((if hilite-use? hilite values)
                    (code fold)))
  (define fold-bind ((if hilite-bind? hilite values)
                     (code fold)))
  (define err-hilite (if show-err?
                         (lambda (p) (hilite p #:color "pink"))
                         values))

  (define main-body
    (code
     .... shop ....
     .... count-down ....))
  (define grocery-body
    (code
     (define (shop l)
       .... #,fold-use ....)))
  (define top-10-body
    (code
     (define (count-down l)
       .... #,(err-hilite fold-use) ....)))
  (define list-body
    (code
     (define #,fold-bind ....)))

  (define sfx (if use? lispy-suffix "scm"))

  (define magazine
    (mk-file #:name "main"
             #:suffix sfx
             #:shape (if use? 'semi-mod 'file)
             (scode #,(if bad-order? load-top-10 load-grocery)
                    #,(if bad-order? load-grocery load-top-10)
                    #,main-body)))

  (define grocery
    (mk-file #:name "grocery"
             #:suffix sfx
             #:shape (if use? 'semi-mod 'file)
             (scode #,load-list
                    #,grocery-body)))
  
  (define top-10
    (let ([p
           (mk-file #:name "top-10"
                    #:suffix sfx
                    #:shape (if use? 'semi-mod 'file)
                    (scode #,((if add-load? values ghost) load-list-too)
                           #,top-10-body))])
      (if show-err?
          (refocus (vr-append (/ gap-size 3)
                              p
                              (colorize (if use?
                                            (it "fold: no binding")
                                            (it "fold: undefined"))
                                        "red"))
                   p)
          p)))
  
  (define lists
    (mk-file #:name "list"
             #:suffix sfx
             #:shape (if use? 'semi-mod 'file)
             (scode #,list-body)))

  (define link-a (if hilite-bad? 0.3 1.0))
  (define load-a 1.0)

  (slide
   #:title (if use?
               "Adding a Namespace Layer"
               "REPL-Oriented Program Structure")
   (let* ([p (vc-append
              (* 3 gap-size)
              (if show-rev?
                  (pin-arrows-line (* a-size 1.5)
                                   magazine
                                   load-grocery (adj lc-find -3 0)
                                   load-top-10 (adj lc-find -3 0)
                                   #:color "red"
                                   #:line-width 7
                                   #:start-angle (- pi)
                                   #:start-pull 1
                                   #:end-angle 0
                                   #:end-pull 1)
                  magazine)
              (hc-append
               (* 3 gap-size)
               grocery
               top-10)
              lists)]
          [p (if show-deps?
                 (let* ([p (link p magazine grocery #:from (adj cb-find (- gap-size) 0) #:alpha link-a)]
                        [p (link p magazine top-10 #:from (adj cb-find gap-size 0) #:to ct*-find #:alpha link-a)]
                        [p (link p grocery lists #:to lt*-find #:alpha link-a)]
                        [p (link p top-10 lists #:to rt-find #:hilite? hilite-bad?)])
                   p)
                 p)]
          [p (if show-loads?
                 (let* ([p (llink p load-grocery grocery #:to ct-find #:from lc-find #:alpha load-a)]
                        [p (llink p (if use? load-top-10-use load-top-10) top-10 #:alpha load-a)]
                        [p (llink p (if use? load-list-use load-list) lists #:to ct*-find #:alpha load-a)]
                        [p (if add-load?
                               (llink p load-list-too lists #:from cb-find #:alpha load-a)
                               p)])
                   p)
                 p)]
          [p (rt-superimpose
              (rt-superimpose
               (inset p 0 0 (* 7 gap-size) 0)
               ((if show-flat? values ghost)
                (scale
                 (hc-append
                  (tt "= ")
                  (mk-file #:shape 'file
                           #:color (make-color 200 230 200)
                           (scode
                            #,((if add-load? values ghost) list-body)
                            #,(if bad-order? top-10-body list-body)
                            #,(if bad-order? list-body grocery-body)
                            #,(if bad-order? grocery-body top-10-body)
                            #,main-body)))
                 0.75)))
              ((if use-note? values ghost)
               (inset
                (note (scale (bright-lightbulb) 1/2) "Use scope to check references")
                0 (* 2 gap-size))))])
     p)))

(define (use-slides #:briefer? [briefer? #f] #:more-direct-deps? [more-direct-deps? #f])
  (unless briefer?
    (lists-slide #:show-loads? #f #:show-deps? #f #:show-flat? #f))
  (lists-slide #:show-deps? #f #:show-flat? #f)
  (lists-slide #:show-deps? #f)
  (unless more-direct-deps?
    (lists-slide #:hilite-use? #t #:show-deps? #f))
  (lists-slide #:hilite-use? #t #:hilite-bind? more-direct-deps?)
  (lists-slide #:bad-order? #t #:show-rev? #t #:show-err? #t)
  (unless briefer?
    (lists-slide #:bad-order? #t #:add-load? #t)
    (lists-slide #:add-load? #t #:use? #t #:use-note? #f))
  (lists-slide #:add-load? #t #:use? #t)
  (unless briefer?
    (lists-slide #:add-load? #f #:show-err? #t #:use? #t)))


(define (eval-when-slide #:gui? [gui? #f]
                         #:eval-when? [eval-when? #f]
                         #:macro? [macro? eval-when?]
                         #:use-means-load? [use-means-load? #f]
                         #:use-means-compile? [use-means-compile? #f]
                         #:unhappy? [unhappy? #f]
                         #:require? [require? #f]
                         #:compile? [compile? (not require?)]
                         #:decl-note? [decl-note? #f]
                         #:require-note? [require-note? #f]
                         #:require-run-note? [require-run-note? #f]
                         #:positive-phrasing? [positive? #f])
  (define a-require (code require))
  (define b-require (code require))
  (define use-grocery (if require?
                          (code (#,a-require "grocery.rkt"))
                          (code (use "grocery.lsp"))))
  (define use-gui (if require?
                      (code (#,b-require "gui.rkt"))
                      (code (use "gui.lsp"))))
  (define groceries-id (code groceries))
  (define init-gui-application!-code 
    (code init-gui-application!))
  (define kitchen-body
    (scode
     #,(cond
        [require?
         (code #,use-grocery
               code:blank)]
        [eval-when?
         ;; hack: want `define'-style indentation:
         (let-syntax ([define (make-code-transformer #'(code eval-when))])
           (code (define (load compile)
                   #,use-grocery)))]
        [else
         (code #,use-grocery
               code:blank)])
     #,(ltl-superimpose
        ((if macro? values ghost)
         (code (shop (#,groceries-id bread
                                     [2 milk] 
                                     apples))))
        ((if macro? ghost values)
         (code (shop ....))))))
  (define grocery-body
    (scode #,((if gui? values ghost)
              (code #,use-gui))
           #,((if macro? values ghost)
              (code
               (define-syntax groceries ....)))
           (define shop ...)
           #,((if gui? values ghost)
              (code
               (define list-editor-gui ....)))))
  (define gui-body
    (scode (#,init-gui-application!-code)
           ....))
  (define (std-size p)
    (inset p 0 0
           (- (max (pict-width kitchen-body)
                   (pict-width grocery-body)
                   (pict-width gui-body))
              (pict-width p))
           0))
  (define shape (if require? 'mod 'semi-mod))
  (define sfx (if require? "rkt" lispy-suffix))
  (define grocery
    (mk-file #:name "grocery"
             #:suffix sfx
             #:shape shape
             (std-size grocery-body)))
  (define gui
    (mk-file #:name "gui"
             #:suffix sfx
             #:shape shape
             (std-size gui-body)))
  (slide
   #:title (cond
            [require? "Using Racket Modules"]
            [macro? "Using Macros"]
            [else "Using Namespaces"])
   (let* ([p (vc-append
              (* 1.5 gap-size)
              (mk-file #:name "kitchen"
                       #:suffix sfx
                       #:shape shape
                       (std-size kitchen-body))
              grocery
              ((if gui? values ghost)
               gui))]
          [p (link p use-grocery grocery
                   #:color load-color
                   #:from lc-find
                   #:to lt*-find
                   #:start-angle (- pi)
                   #:end-angle (* pi -1/6))]
          [p (if gui?
                 (link p use-gui gui
                       #:color load-color
                       #:from lc-find
                       #:to lt*-find
                       #:start-angle (- pi)
                       #:end-angle (* pi -1/6))
                 p)]
          [p (if compile?
                 (refocus (ct-superimpose
                           (para #:width client-w
                                 (nt "Compile") (hbl-append (tt "kitchen.lsp") (nt "...")))
                           (inset p 0 gap-size 0 0))
                          p)
                 p)]
          [p (if use-means-load?
                 (pin-balloon
                  (wrap-balloon (vc-append
                                 gap-size
                                 (apply para #:width (/ client-w 2)
                                        #:fill? #f
                                        (if positive?
                                            (list "Want" (code use) "to load at compile time")
                                            (list "If" (code use) "means run-time" (code load) ":")))
                                 (apply para #:width (/ client-w 2)
                                        #:fill? #f
                                        (if positive?
                                            (list "⇒" (code groceries) "macro is ready")
                                            (list (code groceries) "macro is not ready"))))
                                'nw 0 (- gap-size))
                  p
                  groceries-id cb-find)
                 p)]
          [p (if use-means-compile?
                 (pin-balloon
                  (wrap-balloon (vc-append
                                 gap-size
                                 (apply para #:width (/ client-w 2)
                                        #:fill? #f
                                        (if positive?
                                            (list "Want" (code use) "to run only" (it "current") "time")
                                            (list "If" (code use) "means compile-time" (code load) ":")))
                                 (para #:width (/ client-w 2)
                                       #:fill? #f
                                       (if positive?
                                           "⇒ GUI not initialized during compile"
                                           "GUI initialized during compile")))
                                's 0 gap-size)
                  p
                  init-gui-application!-code (adj lt-find gap-size 0))
                 p)]
          [p (if unhappy?
                 (refocus (rb-superimpose
                           (cc-superimpose p (blank client-w 0))
                           (note (scale (bright-lightbulb) 1/2)
                                 (if gui?
                                     (if positive?
                                         "Scope should cooperate with phases"
                                         "Scope should make times apparent")
                                     "Scope should ensure availability")))
                          p)
                 p)]
          [p (if decl-note?
                 (refocus (rt-superimpose
                           (cc-superimpose p (blank client-w 0))
                           (note #f
                                 #:width (* client-w 1/6)
                                 (vc-append
                                  (current-line-sep)
                                  (t "module")
                                  (it "declaration")
                                  (t "vs.")
                                  (t "module")
                                  (it "instantiation"))))
                          p)
                 p)]
          [p (if require-note?
                 (pin-balloon
                  (wrap-balloon
                   (para #:fill? #f
                         #:width (* client-w 1/3)
                         "At compile time," (code require)
                         "means ``run compile-time code''")
                   'nw 0 (- gap-size))
                  p a-require cb-find)
                 p)]
          [p (if require-run-note?
                 (pin-balloon
                  (wrap-balloon
                   (para #:fill? #f
                         #:width (* client-w 1/3)
                         "At run time," (code require)
                         "means ``run run-time code''")
                   'nw 0 (- gap-size))
                  p b-require cb-find)
                 p)])
     p)))

(define (eval-when-slides #:briefer? [briefer? #f]
                          #:positive-phrasing? [positive? #f]
                          #:run-time-note? [run-time-note? #f])
  (unless briefer?
    (eval-when-slide #:compile? #f))
  (eval-when-slide #:macro? #t #:compile? #f)
  (unless briefer?
    (eval-when-slide #:macro? #t))
  (eval-when-slide #:macro? #t #:use-means-load? #t #:positive-phrasing? positive? #:compile? (not briefer?))
  (eval-when-slide #:macro? #t #:use-means-load? #t #:unhappy? #t #:positive-phrasing? positive? #:compile? (not briefer?))
  (eval-when-slide #:gui? #t #:compile? (not briefer?))
  (eval-when-slide #:gui? #t #:use-means-compile? #t #:positive-phrasing? positive? #:compile? (not briefer?))
  (eval-when-slide #:gui? #t #:use-means-compile? #t #:unhappy? #t #:positive-phrasing? positive? #:compile? (not briefer?))
  (eval-when-slide #:gui? #t #:require? #t #:macro? run-time-note?)
  #; (eval-when-slide #:gui? #t #:require? #t #:decl-note? #t)
  (eval-when-slide #:gui? #t #:require? #t #:require-note? #t #:macro? run-time-note?)
  (when run-time-note?
    (eval-when-slide #:gui? #t #:require? #t #:require-note? #t #:require-run-note? #t #:macro? #t))
  #;(eval-when-slide #:gui? #t #:require? #t))

(define (phase-slide #:run-time? [run-time? #t]
                     #:run-time-arrows? [run-time-arrows? run-time?]
                     #:compile-time? [compile-time? #t]
                     #:compile-time-arrows? [compile-time-arrows? compile-time?]
                     #:template? [template? #f]
                     #:split? [split? #f]
                     #:both? [both? split?])

  (define (r-encloud #:dh [dh 0] p)
    (if run-time?
        (encloud #:dh dh p)
        p))
  (define (c-encloud #:dh [dh 0] p)
    (if compile-time?
        (encloud #:dh dh
                 #:color comptime-color
                 p) 
        p))

  (define grocery-ref (code "grocery.rkt"))
  (define list-ref (code "list.rkt"))
  (define list-ref2 (launder list-ref))
  (define list-stx-ref (code (for-syntax "list.rkt")))

  (define kitchen-body
    (scode
     (require #,list-ref
              #,grocery-ref)
     (define weekly #,(r-encloud (code (groceries ....))))
     #,(r-encloud (code (shop .... fold ....)))))
  (define fold (hbl-append ((if template? values ghost) (tt "#'"))
                           (let ([f (code fold)])
                             (if template?
                                 (r-encloud #:dh -4 f)
                                 f))))
  (define grocery-body
    (scode
     (require "gui.rkt"
              #,(if template?
                    list-ref2
                    (if both?
                        (code #,list-ref2 #,list-stx-ref)
                        (code #,list-stx-ref))))
     (define-syntax groceries #,(c-encloud (code .... #,fold ....))) #,(ghost (code 1))
     (define shop #,(r-encloud (if (or template? both?)
                                   (code .... fold ....)
                                   (code ....))))
     (define list-editor-gui #,(r-encloud (code ....)))))
  (define list-body
    (scode
     (define fold #,(r-encloud (code ....)))))
  (define (std-size p)
    (inset p 0 0
           (- (max (pict-width kitchen-body)
                   (pict-width grocery-body)
                   (pict-width list-body))
              (pict-width p))
           0))

  (define spacer (blank gap-size 0))

  (define kitchen-mod
    (mk-file #:name "kitchen"
             #:suffix "rkt"
             (std-size kitchen-body)))
  (define grocery-mod
    (mk-file #:name "grocery"
             #:suffix "rkt"
             (std-size grocery-body)))
  (define core-list-mod
    (mk-file #:name "list"
             #:suffix "rkt"
             (std-size list-body)))
  (define list-mod2 ((if split? values ghost) (launder core-list-mod)))
  (define list-mod
    (refocus (ct-superimpose
              (inset list-mod2 gap-size gap-size 0 0)
              core-list-mod)
             core-list-mod))

  (slide
   #:title "Scope and Phases"
   (ct-superimpose
    (inset
     (para #:width client-w
           spacer
           ((if run-time? values ghost)
            (hbl-append (encloud (tt " ")) (t " = run time")))
           spacer
           ((if compile-time? values ghost)
            (hbl-append
             (encloud (tt " ") #:color comptime-color) (t " = compile time"))))
     0 (* -3/4 gap-size) 0 0)
    (let* ([p (vc-append
               gap-size
               (blank)
               kitchen-mod
               grocery-mod
               list-mod)]
           [a (/ gap-size 2)]
           [lw 3]
           [p (if run-time-arrows?
                  (pin-arrow-line a p
                                  list-ref lc-find
                                  list-mod lt*-find
                                  #:start-angle (* pi -9/10)
                                  #:start-pull 0.75
                                  #:end-angle (* pi -1/6)
                                  #:end-pull 1/2
                                  #:line-width lw
                                  #:color runtime-color)
                  p)]
           [p (if run-time-arrows?
                  (pin-arrow-line a p
                                  grocery-ref lc-find
                                  grocery-mod lt*-find
                                  #:start-angle (* pi -9/10)
                                  #:start-pull 1
                                  #:end-angle (* pi -1/6)
                                  #:end-pull 1/2
                                  #:line-width lw
                                  #:color runtime-color)
                  p)]
           [p (if (and (or template? both?)
                       run-time-arrows?)
                  (pin-arrow-line a p
                                  list-ref2 lc-find
                                  list-mod lt*-find
                                  #:start-angle (* pi -9/10)
                                  #:start-pull 0.75
                                  #:end-angle (* pi -1/6)
                                  #:end-pull 1/2
                                  #:line-width lw
                                  #:color runtime-color)
                  p)]
           [p (if (or template?
                      (not compile-time-arrows?))
                  p
                  (pin-arrow-line a p
                                  list-stx-ref rc-find
                                  (if split? list-mod2 list-mod) rt-find
                                  #:start-angle 0
                                  #:start-pull 1
                                  #:end-angle (* pi -5/6)
                                  #:end-pull 1/2
                                  #:line-width lw
                                  #:color comptime-color))])
      p))))

(define (phase-slides #:extras? [extras? #f])
  (phase-slide #:run-time? #f #:compile-time? #f)
  (phase-slide #:run-time-arrows? #f #:compile-time? #f)
  (phase-slide #:compile-time? #f)
  (phase-slide #:compile-time-arrows? #f)
  (phase-slide)
  (phase-slide #:template? #t)
  (phase-slide #:both? #t)
  (phase-slide #:split? #t))

(module+ main
  (use-slides)
  (eval-when-slides)
  (phase-slides))
