#lang slideshow
(require racket/runtime-path
         slideshow/code
         slideshow/face
         slideshow/balloon
         racket/class
         racket/draw
         "in-file.rkt"
         "style.rkt")

(provide story-slides
         script-slides
         module-phase-slides)

(define require-color runtime-color)
(define require-for-syntax-color comptime-color)

(define-runtime-path mm-dir "../mmtalk")

(define (bget base-name)
  (build-path mm-dir
              (format "~a.bmp" base-name)))

(define emacs-scheme (bitmap (bget "emacs-scheme")))
(define emacs-macros (bitmap (bget "emacs-macros")))
(define emacs-mzc (bitmap (bget "emacs-mzc")))
(define emacs-eval-when (bitmap (bget "emacs-eval-when")))
(define emacs-lib (bitmap (bget "emacs-lib")))

(define (ott s) (text s 
                      `(no-combine
                        . ,(make-font #:face "Courier" 
                                      #:smoothing 'unsmoothed
                                      #:size (current-font-size)))
                      (current-font-size)))

(define (->bitmap p)
  (define bm (make-platform-bitmap (inexact->exact (floor (pict-width p)))
                                   (inexact->exact (floor (pict-height p)))))
  ((make-pict-drawer p) (send bm make-dc) 0 0)
  (bitmap bm))

(define (synthesize top bottom)
  (define (white w h) (colorize (filled-rectangle w h) "white"))
  (let ([p (pin-over emacs-lib 17 27
                     (lt-superimpose (white 455 150)
                                     (->bitmap top)))])
    (pin-over p 17 217
              (lt-superimpose (white 455 150)
                              (->bitmap bottom)))))

(define telnet (synthesize
                (vl-append
                 6
                 (ott "(eval-when ...")
                 (ott " (load \"gparselib.scm\"))")
                 (ott "(define (parse file)")
                 (ott "  ...)"))
                (vl-append
                 (current-line-sep)
                 (ott "$ mzc parse.scm")
                 (ott "Can't open display: :0.0")
                 (hbl-append (ott "$ ") (lift-above-baseline
                                         (inset
                                          (colorize (filled-rectangle 19 38)
                                                    (make-color 90 90 90))
                                          0 2 0 0)
                                         -13)))))

(define (face-slide title face screen)
  (slide
   #:title title
   (hc-append 
    (* 3 gap-size)
    face
    screen)))

(define font-size (current-font-size))
(define (alg-code s) (tt s))

(define (3/4t-find in p)
  (let-values ([(x y) (ct-find in p)])
    (values (+ x (* 1/4 (pict-width p))) y)))

;; ----------------------------------------

(define (story-slides #:face [face face]
                      #:face* [face* face*]
                      #:skip-intro? [skip-intro? #f]
                      #:post-intro-title [post-intro-title "... Programming in MzScheme"]
                      #:in-mzscheme-title [in-mzscheme-title "... in Macro-Extended MzScheme!"])
  (unless skip-intro? 
    (face-slide "A MzScheme Programmer (ca. 2000)..." (face 'happy) (ghost emacs-scheme)))
  (face-slide post-intro-title (face 'happier) emacs-scheme)
  (face-slide in-mzscheme-title (face 'happiest) emacs-macros)
  (face-slide "... Trying to Use the Compiler" (face 'sortof-unhappy) emacs-mzc)
  (face-slide "... Accomodating the Compiler" (face 'unhappy) emacs-eval-when)
  (face-slide "... Trying a Complex Library" (face 'badly-embarassed) emacs-lib)
  (face-slide "... Compiling Remotely" (face* 'worried 'large #t default-face-color 3) telnet))

;; ----------------------------------------

(define (mk-load name indent?)
  (code (load #,(colorize (tt (format "\"~a.scm\"" name))
                          (current-literal-color)))))

(define load-a-expr (mk-load "a" #f))
(define load-b-expr (mk-load "b" #f))
(define load-m-expr (mk-load "m" #t))
(define load-u-expr (mk-load "u" #f))
(define load-u-expr2 (launder load-u-expr))
(define load-ma-expr (mk-load "ma" #f))
(define load-mb-expr (mk-load "mb" #f))

(define orig-file
  (mk-file 
   #:name "program"
   (vl-append
    (current-line-sep)
    load-u-expr2
    (code (eval-when
           (compile load)
           #,load-m-expr))
    load-a-expr
    load-b-expr
    (code ....))))

(define (file-label name)
  (colorize (t name) load-color))
  
(define (mk-small-content name)
  (vl-append
   (current-line-sep)
   (ghost (tt "    "))))

(define (mk-m-file u?)
  (mk-file 
   #:name "m"
   (vl-append
    (current-line-sep)
    #;
    (if u?
        (colorize load-u-expr "red")
        (tt " "))
    load-ma-expr
    load-mb-expr)))

(define a-file (mk-file #:name "a" (mk-small-content "a")))
(define b-file (mk-file #:name "b" (mk-small-content "b")))

(define u-file (mk-file #:name "u" (mk-small-content "u")))
(define ma-file (mk-file #:name "ma" (mk-small-content "ma")))
(define mb-file (mk-file #:name "mb" (mk-small-content "mb")))

(define connect-loads
  (case-lambda
   [(p color find-ref find-mod) p]
   [(p color find-ref find-mod from to . rest)
    (apply connect-loads
           (pin-arrow-line 
            (/ font-size 2)
            p
            from find-ref
            to find-mod
            #:line-width 3 
            #:color color)
           color find-ref find-mod
           rest)]))

(define small-face-scale 1/3)
(define small-unhappy (scale (face 'unhappy) small-face-scale small-face-scale))
(define small-happy (scale (face 'happy) small-face-scale small-face-scale))

(define loaded-none
  (wrap-balloon (hc-append
                 font-size
                 small-unhappy
                 (vl-append
                  (current-line-sep)
                  (t "never loaded")
                  (t "in compile mode")))
                'e 30 0))

(define loaded-twice
  (wrap-balloon (hc-append
                 font-size
                 small-unhappy
                 (vl-append
                  (current-line-sep)
                  (t "loaded twice")
                  (t "in interactive mode")))
                'ne 30 -50))

(define (demo-arrow color)
  (let ([p (blank (* 2 font-size) font-size)])
    (connect-loads p color lc-find rc-find p p)))

(define (mk-script-ex deps? deps2? no-load? m-to-u? x2?)
  (define m-file (mk-m-file m-to-u?))
  (slide
   #:title "The Trouble with Scripting the Compiler"
   (let* ([lc-color (if deps?
                        (scale-color 4.0 load-color)
                        load-color)]
          [p (connect-loads
              (connect-loads
               (connect-loads
                (ht-append
                 (* 3 font-size)
                 orig-file
                 (vl-append
                  (* 1.5 font-size)
                  (blank)
                  (blank)
                  m-file
                  a-file
                  b-file)
                 (vl-append
                  (* 2 font-size)
                  u-file
                  ma-file
                  mb-file))
                lc-color
                rc-find lt*-find
                load-a-expr a-file
                load-b-expr b-file
                load-ma-expr ma-file
                load-mb-expr mb-file)
               lc-color
               rc-find lc-find
               load-m-expr m-file)
              (if x2? load-color lc-color)
              rc-find lt*-find
              load-u-expr2 u-file)]
          [p (if m-to-u?
                 (connect-loads p load-color rc-find lc-find load-u-expr u-file)
                 p)]
          [p (if (and deps? (not x2?))
                 (if deps2?
                     (connect-loads p require-color
                                    ct-find cb-find
                                    ma-file u-file)
                     (connect-loads
                      (connect-loads
                       (connect-loads p require-color
                                      ct-find cb-find
                                      b-file a-file
                                      mb-file ma-file)
                       require-color ct-find lb*-find
                       a-file m-file)
                      require-color ct-find lb*-find
                      a-file u-file))
                 p)]
          [p (if x2?
                 (pin-balloon loaded-twice p u-file cb-find)
                 p)]
          [p (if no-load?
                 (pin-balloon loaded-none p u-file lc-find)
                 p)]
          [p (if (not deps?)
                 (rb-superimpose
                  p
                  (hc-append (demo-arrow load-color) (t " = load effect")))
                 p)]
          [p (if deps?
                 (rb-superimpose
                  p
                  (hc-append (demo-arrow require-color) (t " = dependency")))
                 p)])            
     p)))

(define (script-slides)
  (mk-script-ex #f #f #f #f #f)
  (mk-script-ex #t #f #f #f #f)
  (mk-script-ex #t #t #f #f #f)
  (mk-script-ex #t #t #t #f #f)
  ;; (mk-script-ex #t #t #f #t #f)
  ;; (mk-script-ex #t #t #f #t #t)
  (void))

;; ----------------------------------------

(define pale-require-color (scale-color #e3 (make-object color% require-color)))
(define pale-require-for-syntax-color (scale-color #e3 (make-object color% require-for-syntax-color)))

(define (mk-req name for-syntax?)
  (colorize (let ([name (colorize (tt (format "\"~a.rkt\"" name))
                                  (current-literal-color))])
              (if for-syntax?
                  (code (require (for-syntax #,name)))
                  (code (require #,name))))
            (if for-syntax?
                require-for-syntax-color
                require-color)))

(define (cloud-bg color)
  (lambda (p) 
    (let ([d (pict-descent p)])
      (lbl-superimpose
       (lift-above-baseline (cloud (pict-width p) (pict-height p) color)
             (- d))
       (lift-above-baseline (inset (colorize p "white") -1 0 0 -1) (- 1))
       p))))

(define dots (code ....))

(define (mk-a-module name vfactor add-end? body-color file-color border-color . lines)
  (mk-file
   #:name name
   #:suffix "rkt"
   (scale
    (apply
     vl-append
     (current-line-sep)
     (append
      lines
      (if add-end?
          (list ((if body-color (cloud-bg body-color) values) dots))
          null)))
    0.8)
   file-color
   vfactor
   border-color))

(define (mk-module name body-color . lines)
  (apply mk-a-module name vfactor #t body-color #t "black" lines))

(define rt-blob ((cloud-bg require-color) (ghost dots)))
(define ct-blob ((cloud-bg require-for-syntax-color) (ghost dots)))

(define overview-steps
  '(a a-rt a-ct a-ctx b b-rt b-ct u au split))

(define (mk-modules step)
  (define (after p)
    (memq step (or (memq p overview-steps) null)))
  (define (between p1 p2)
    (and (after p1) (or (eq? step p2) (not (after p2)))))
  (define (between-excl p1 p2)
    (and (after p1) (not (after p2))))
  
  (define (vafter p)
    (if (after p)
        values
        ghost))

  (define require-u (mk-req "u" #f))
  (define require-a (mk-req "a" #f))
  (define require-for-syntax-u (mk-req "u" #t))

  (define mk-define
    (lambda (name stx? color? [both? #f])
      (let ([rhs ((if color?
                      (cloud-bg (if stx?
                                    require-for-syntax-color
                                    require-color))
                      values)
                  dots)]
            [name (colorize (tt name) (current-id-color))])
        (let ([def (if stx?
                       (code (define-syntax #,name #,rhs))
                       (code (define #,name #,rhs)))])
          (if both?
              (values def rhs)
              def)))))
  
  (define b-module
    (mk-module
     "b" (and (after 'b-rt) require-color)
     require-a
     ((vafter 'u) require-for-syntax-u)
     (mk-define "n" #t (after 'b-ct))
     (mk-define "g" #f (after 'b-rt))))
  
  (define a-rt? (after 'a-rt))
  
  (define-values (a-mod-macro-def a-mod-macro-body)
    (mk-define "m" #t (after 'a-ct) #t))
  
  (define a-module
    (apply
     mk-module
     "a" (and a-rt? require-color)
     (append
      (if (after 'au)
          (list require-u)
          null)
      (list
       a-mod-macro-def
       (mk-define "f" #f a-rt?)))))

  (define a-balloon
    (let ([e
           (lambda (cloud?)
             (scale
              (colorize
               (code #'#,((if cloud?
                              (cloud-bg require-color) 
                              values)
                          (colorize (code (f ...)) "lightgray")))
               "lightgray")
              0.8))])
      (define (w-e cloud?)
        (parameterize ([code-colorize-enabled #f])
          (e cloud?)))
      (wrap-balloon
       (lt-superimpose
        (inset (w-e #t) 1 1 0 0)
        (inset (w-e #f) -1 -1 0 0)
        (e #f))
       'se 20 70)))
  
  (define (mk-u-module #:name [name "u"] body-color)
    (mk-module
     name #f
     (mk-define "fold" #f #f)))
  
  (define u-module (mk-u-module
                    (if (after 'split)
                        (or #t pale-require-color)
                        #t)))
  (define u-split-module 
    (inset (if (after 'split)
               (mk-u-module (or #t pale-require-for-syntax-color))
               (ghost (launder u-module)))
           font-size font-size
           0 0))

  (define (inset-blurb p)
    (inset p 0 font-size 0 0))

  (define p
    (vc-append
     (* 2 font-size)
     (lt-superimpose
      u-split-module
      ((vafter 'u) u-module))
     (ht-append
      (* 2 font-size)
      a-module
      ((vafter 'b) b-module))))
  
  (slide
   #:title "Modules and Macros"
   (blank gap-size)
   (let* ([p (if (after 'b)
                 (connect-loads
                  p require-color lc-find rc-find
                  require-a a-module)
                 p)]
          [p (if (after 'u)
                 (connect-loads
                  p require-for-syntax-color 3/4t-find rb*-find
                  require-for-syntax-u (if (after 'split)
                                           u-split-module 
                                           u-module))
                 p)]
          [p (if (after 'au)
                 (connect-loads
                  p require-color 3/4t-find lb*-find
                  require-u u-module)
                 p)]
          [p (if (between-excl 'a-ctx 'au)
                 (pin-balloon a-balloon p a-mod-macro-body ct-find)
                 p)]
          [p (if (between 'a-rt 'a-rt)
                 (lt-superimpose
                  (inset-blurb
                   (hbl-append rt-blob (t " = run-time expressions")))
                  p)
                 p)]
          [p (if (between 'a-ct 'a-ct)
                 (lt-superimpose
                  (inset-blurb
                   (hbl-append ct-blob (t " = compile-time expressions")))
                  p)
                 p)]
          [p (if (between 'a-ctx 'a-ctx)
                 (lt-superimpose
                  (inset-blurb
                   (hbl-append (colorize (tt "#'") (current-literal-color))
                               (t " in ") ct-blob
                               (t " escapes back to ") rt-blob))
                  p)
                 p)]
          [p (if (between 'split 'split)
                 (pin-balloon
                  (wrap-balloon
                   (vl-append
                    (current-line-sep)
                    (t "different instance")
                    (t "for each phase"))
                   'sw -30 0)
                  p u-split-module rc-find)
                 p)])
     p)))

(define (module-phase-slides)
  (for-each mk-modules overview-steps))

;; ----------------------------------------

(module+ main
  (story-slides)
  (script-slides)
  (module-phase-slides))
