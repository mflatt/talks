#lang slideshow
(require slideshow/code
         slideshow/balloon
         slideshow/face
         slideshow/flash
         racket/draw
         slideshow/play
         "utils.rkt")

(provide what-is-a-macro-slides
         dsl-idea-slides
         parsing-slides
         phase-slides
         rep-code-slides

         pipeline-pict
         make-pipeline)

(define (what-is-a-macro-slides)
  ;; ----------------------------------------
  
  (define (plus #:color [col "blue"] a b)
    (round-wrap (hc-append gap-size a (colorize (arrowhead gap-size 0) "purple") b)
                #:color col))
  
  (define fc-scale 0.65)
  
  (define preprocessors-vs-macros "Preprocessors vs. Macros")
  
  (define (evenize p) p)
  
  (define (ant n n2)
    (let* ([house-defn-o (vl-append (tt "house =") (tt " ..."))]
           [house-defn (scale house-defn-o fc-scale)]
           [house-defn1 (ghost (launder house-defn-o))]
           [house-defn2 (ghost (launder house-defn-o))]
           [game-java
            (as-file (fade-pict n
                                (tt "game.maze") 
                                (tt "game.java") )
                     (scale (vl-append (tt "import") 
                                       (tt " java.io.*")
                                       (fade-pict n
                                                  (tt " ")
                                                  (tt " house.*"))
                                       house-defn1)
                            fc-scale))]
           [game-java1 (ghost (launder game-java))]
           [game-java2 (ghost (launder game-java))]
           [home-java (as-file (tt "house.maze") (scale house-defn2
                                                        fc-scale))]
           [mazec (plus (fade-pict n
                                   game-java1
                                   home-java)
                        (tt "mazec"))]
           [mazec1 (ghost (launder mazec))]
           [mazec2 (ghost (launder mazec))]
           [game-comp
            (plus (fade-pict n
                             mazec1
                             game-java2)
                  (tt "javac"))]
           [home-comp
            (fade-pict
             n
             (blank)
             (plus mazec2 (tt "javac")))]
           [all
            (hc-append
             gap-size
             (evenize
              (vc-append
               gap-size
               game-comp
               (fade-pict n (blank) home-comp)))
             (evenize
              (fade-pict n2
                         (blank)
                         (plus #:color "green" (as-file (tt "maze.java") (blank)) (tt "java")))))])
      (slide-pict
       (slide-pict (slide-pict all mazec mazec1 mazec2 n)
                   game-java game-java1 game-java2 
                   n)
       house-defn house-defn1 house-defn2        
       n)))
  
  (play-n 
   #:title preprocessors-vs-macros
   ant)
  
  (define schemec (tt "raco make"))

  (call-with-gauge 
   gauge:lisp0
   (lambda ()
     (play-n
      #:title preprocessors-vs-macros
      (lambda (n1 n2)
        (let* ([placedef-o
                (code (define-syntax define-place
                        ...))]
               [house-o
                (code (define-place house ....))]
               [maze-ss-str-o (code "maze.rkt")]
               [placedef (scale placedef-o fc-scale)]
               [house (scale house-o fc-scale)]
               [maze-ss-str (cellophane (scale maze-ss-str-o fc-scale) n1)]
               [placedef1 (launder (ghost placedef-o))]
               [placedef2 (launder (ghost placedef-o))]
               [house1 (launder (ghost house-o))]
               [house2 (launder (ghost house-o))]
               [maze-ss-str1 (launder (ghost maze-ss-str-o))]
               [maze-ss-str2 (launder (ghost maze-ss-str-o))]
               [maze-lib
                (plus
                 (as-file (tt "maze.rkt") 
                          (scale placedef2
                                 fc-scale))
                 schemec)]
               [house-lib
                (plus
                 (as-file (tt "house.rkt") 
                          (scale (code (require #,maze-ss-str2)
                                       code:blank
                                       #,house2)
                                 fc-scale))
                 schemec)]
               [game-lib
                (plus
                 (as-file (tt "game.rkt") 
                          (scale (code #,(hbl-append
                                          (htl-append
                                           (ptt "(")
                                           (code require)
                                           (tt " ")
                                           (fade-pict n1
                                                      (code "io.rkt")
                                                      (vl-append
                                                       (current-code-line-sep)
                                                       (code "io.rkt")
                                                       (fade-pict n2
                                                                  maze-ss-str1
                                                                  (code "house.rkt")))))
                                          (ptt ")"))
                                       #,(fade-pict
                                          n1
                                          (code code:blank
                                                #,placedef1
                                                code:blank)
                                          (code code:blank))
                                       #,(fade-pict
                                          n2
                                          house1
                                          (blank)))
                                 fc-scale))
                 schemec)]
               [mods               
                (vc-append
                 gap-size
                 game-lib
                 (fade-pict
                  n2
                  (blank)
                  house-lib)
                 (fade-pict
                  n1
                  (blank)
                  maze-lib))])
          (slide-pict
           (slide-pict
            (slide-pict mods
                        placedef placedef1 placedef2
                        n1)
            house house1 house2
            n2)
           maze-ss-str maze-ss-str1 maze-ss-str2
           n2)))))))

;; ----------------------------------------
  
(define (pin-arrows-line* sz p src src-find dest dest-find #:line-width lw #:color col)
  (let-values ([(sx sy) (src-find p src)]
               [(dx dy) (dest-find p dest)]
               [(APART) (* gap-size -4)])
    (pin-over 
     p
     0 0
     (colorize
      (linewidth
       lw
       (let* ([p
               (dc (lambda (dc x y)
                     (let ([b (send dc get-brush)])
                       (send dc set-brush "white" 'transparent)
                       (let ([draw
                              (lambda (del)
                                (let ([path (new dc-path%)]
                                      [dh (/ (- dy sy) 4)])
                                  (send path move-to sx sy)
                                  (send path curve-to 
                                        (del dx (/ APART 2)) (+ sy dh)
                                        (del dx (/ APART 2)) (- dy dh)
                                        dx dy)
                                  (send dc draw-path path (- x (del (/ gap-size 2))) y)))])
                         (draw -)
                         (draw +))
                       (send dc set-brush b)))
                   0 0)]
              [p (pin-arrow-line sz
                                 p
                                 p (lambda x (values (- dx (/ gap-size 2) 2) (+ dy 0)))
                                 p (lambda x (values (- dx (/ gap-size 2) -2) (- dy 4))))]
              [p (pin-arrow-line sz
                                 p
                                 p (lambda x (values (+ sx (/ gap-size 2) 2) (- sy 0)))
                                 p (lambda x (values (+ sx (/ gap-size 2) -2) (+ sy 4))))])
         p))              
      col))))
  
(define ((decorate-arrow sep show-top top show-bottom bottom) trans-arrow)
  (refocus
   (let* ([arr (inset trans-arrow 0 (/ gap-size 2))]
          [p (vc-append
              (* sep gap-size)
              (show-top top)
              arr
              (show-bottom bottom))])
     (let ([pin-one
            (lambda (show p pin-arrow-line end end-find arr-find)
              (cc-superimpose
               p
               (show
                (pin-arrow-line (/ gap-size 2)
                                (ghost p )
                                end end-find
                                arr arr-find
                                #:line-width 2
                                #:color "red"))))])
       (let* ([p (pin-one show-top p pin-arrow-line
                          top cb-find ct-find)]
              [p (pin-one show-bottom p pin-arrows-line*
                          bottom ct-find cb-find)])
         p)))
   trans-arrow))

(define (xform lang reader mod req left-adj right-adj
               #:require-code [require-code (code (require _path))])
  (pipeline
   left-adj
   (decorate-arrow 6 lang (code #,(tt "#lang") _path) reader (code #,(tt "#reader") _path))
   values
   (decorate-arrow 4 mod (code (module _name _path ...)) req require-code)
   right-adj))
  
(define pipeline-pict (xform values values values values values values))

(define (make-pipeline #:reader? [reader? #f]
                       #:req? [req? reader?]
                       #:mod? [mod? req?]
                       #:lang? [lang? mod?])
  (xform (if lang? values ghost)
         (if reader? values ghost)
         (if mod? values ghost)
         (if req? values ghost)
         values values))
  
;; ----------------------------------------

(define (dsl-idea-slides #:title parsing-title)

  (define require-code (vc-append
                        10
                        (code (define-syntax _macro ....))
                        (code (require _path))))
  
  (define (xform-slide lang reader mod req . top)
    (slide
     #:title parsing-title
     (blank (* 3 gap-size))
     (xform lang reader mod req values values
            #:require-code require-code)
     (blank (* 2 gap-size))
     (ct-superimpose (apply vc-append gap-size top)
                     (blank (* 1/3 client-h)))))
  
  (xform-slide ghost ghost ghost ghost
               (blank))
  (xform-slide values ghost ghost ghost
               (vc-append
                (current-line-sep)
                (item (tt "#lang") "line determines concrete syntax")
                (subitem #:bullet (ghost bullet) "parser implemented by module at" (code _path)))
               (item "Result is a parenthesized module"))
  (xform-slide values ghost values ghost
               (item "Module's import" (code _path) "determines rewrite into AST"))
  (xform-slide values ghost values values
               (ghost (item "filler"))
               (blank)
               (vc-append
                (/ gap-size 2)
                (item "Conversion is extensible via macros + more imports")
                (para #:fill? #f (t "⇒") "composable and convenient for many DSLs"))))

;; ----------------------------------------

(define (parsing-slides)

  (define parsing-title "Parsing")
  
  (define (xform-slide lang reader mod req level)
    (slide/gauge
     #:level level
     #:title parsing-title
     (xform lang reader mod req values values)))
  
  (xform-slide ghost ghost ghost ghost gauge:basic-lisp)
  (xform-slide values ghost ghost ghost gauge:taste-of-plt0)
  (xform-slide values ghost values ghost gauge:taste-of-scheme)
  (xform-slide values ghost values values gauge:taste-of-plt)
  (xform-slide values values values values gauge:taste-of-plt)
  
  ;; ----------------------------------------
  
  (define scheme-orig
    (as-file*
     (code #,(tt "#lang") scheme
           (define (hi)
             "Hello"))))
  (define scheme-mod
    (code (module m scheme
            (define (hi)
              "Hello"))))
  (define scheme-core
    (record
     (rec-sub
      (record
       (rec-tt "define")
       (ht-append
        gap-size
        (rec-tt "hi")
        (rec-sub
         (record
          (rec-tt "function")
          (rec-tt "()")
          (rec-tt "\"Hello\""))))))))
  
  (define scribble-orig
    (as-file*
     (vl-append
      (current-code-line-sep)
      (code #,(tt "#lang") scribble/doc)
      (htl-append (tt "@") (code (require 
                                  scribble/manual)))
      (hbl-append (tt "@")
                  (code bold)
                  (ptt "{")
                  (colorize (tt "Hi") (current-literal-color))
                  (ptt "}")))))
  (define scribble-mod
    (code
     (module m doclang
       (require 
        scribble/manual)
       (bold "Hi"))))
  (define scribble-core
    (record
     (rec-sub
      (ht-append
       gap-size
       (vl-append
        (/ gap-size 2)
        (record
         (rec-tt "import")
         (rec-tt "scribble/doc")
         (rec-tt "scribble/manual"))
        (record
         (rec-tt "export")
         (rec-tt "doc")))
       (record
        (rec-tt "define")
        (rec-tt "doc")
        (rec-sub
         (record
          (rec-tt "apply")
          (rec-tt "bold")
          (rec-tt "(\"Hi\")"))))))))
  
  (define honu-orig
    (as-file*
     (vl-append
      (current-code-line-sep)
      (code #,(tt "#lang") honu)
      (hbl-append (code 1) (code +) (code 2) (ptt ";")))))
  (define honu-mod
    (code (module m honu
            1 + 2 #,(colorize (tt "|;|") (current-id-color)))))
  (define honu-core
    (record
     (rec-sub
      (ht-append
       gap-size
       (record
        (rec-tt "import")
        (rec-tt "honu-procs"))
       (record
        (rec-tt "apply")
        (rec-tt "print")
        (rec-sub
         (record
          (rec-tt "apply")
          (rec-tt "+")
          (rec-tt "(1 2)"))))))))
  
  (slide/gauge
   #:level gauge:taste-of-plt
   #:title parsing-title
   (scale
    (table
     5
     (let ([trans-arrow (inset (scale trans-arrow 0.5)
                               0 gap-size 0 0)])
       (list scheme-orig trans-arrow scheme-mod trans-arrow scheme-core
             scribble-orig trans-arrow scribble-mod trans-arrow scribble-core
             honu-orig trans-arrow honu-mod trans-arrow honu-core))
     lc-superimpose lc-superimpose
     (* 1.5 gap-size) (* 3 gap-size))
    0.75))
  
  ;; ----------------------------------------
  
  (define-syntax-rule (id s)
    (colorize (tt (symbol->string 's)) (current-id-color)))
  
  (slide/gauge
   #:level gauge:more-lisp
   #:title parsing-title
   'alts
   (list 
    (list (xform ghost ghost ghost ghost values values))
    (list
     (xform ghost ghost ghost ghost values fade)
     (blank)
     (blank)
     (para #:align 'center (it "Read") "layer provides absolute control")
     (blank)
     (scale
      (hc-append (* 3 gap-size)
                 (code (+ 1 2)) 
                 (hbl-append (tt "@") 
                             (code bold) 
                             (ptt "{") 
                             (colorize (tt "Hi") (current-literal-color))
                             (ptt "}") )
                 (hbl-append (code 1) (code +) (code 2)))
      0.75))
    (list
     (xform ghost ghost ghost ghost fade values)
     (blank)
     (blank)
     (para #:align 'center (it "Expand") "layer can delay ``inside'' until after ``outside''")
     (blank)
     (ht-append
      (* 4 gap-size)
      (scale
       (code
        (define-place start .... 
          ([north house-front]
           [south desert]))
        code:blank
        (define-place house-front .... 
          ([in room]
           [south start])))
       0.75)
      (scale
       (code int #,(hbl-append (id is_odd) (code (int x))) #,(ptt "{")
             code:blank ... #,(hbl-append (id is_even) (code (#,(hbl-append (code x) (code -) (code 1)))) (ptt ";"))
             #,(ptt "}")
             code:blank
             int #,(hbl-append (id is_even) (code (int x))) #,(ptt "{")
             code:blank ... #,(hbl-append (id is_odd) (code (#,(hbl-append (code x) (code -) (code 1)))) (ptt ";"))
             #,(ptt "}"))
       0.75))))))

;; ================================================================================

(define (phase-slides)
  ;; ----------------------------------------
  
  (define (bg color p)
    (refocus (cc-superimpose
              (colorize (filled-rounded-rectangle (pict-width p) (pict-height p) 4)
                        color)
              p)
             p))
  
  (define (compile-time p)
    (bg (make-object color% 200 255 200) p))
  
  (define (bridge-time p)
    (bg (make-object color% 255 255 200) p))
  
  (define (rbridge-time p)
    #;(bg (make-object color% 255 200 255) p)
    (bridge-time p))
  
  (define (run-time p)
    (bg "white" p))
  
  (define (inset-compile p) (inset p 2))
  
  (define expand-time-title "Expressions, Bindings, and Phases")
  
  (define (exp-time-expr #:c [compile-time compile-time] #:b [bridge-time bridge-time] show-langs)
    (slide/gauge
     #:level gauge:and-more-lisp
     #:title expand-time-title
     
     (show-langs (item "Lisp/Scheme:"))
     (code (#,(bridge-time (code define-syntax)) three
            #,(hbl-append (tt " ")
                          (compile-time (inset-compile (code (lambda (stx) #'#,(run-time (code 3))))))))
           code:blank
           (+ 1 (three)))
     
     (show-langs (item "PL/I"))
     (show-langs
      (vl-append (current-code-line-sep)
                 (hbl-append (bridge-time (tt "%")) (tt "Three : procedure"))
                 (hbl-append (tt "  ")
                             (compile-time (tt "ANS( 3 );")))
                 (hbl-append (bridge-time (tt "%")) (tt "end"))
                 (tt "1 + Three")))
     
     (show-langs (item "Template Haskell:"))
     (show-langs (vl-append (current-code-line-sep)
                            (tt "three s = [| 3 |]")
                            (hbl-append (bridge-time (tt "$("))
                                        (compile-time (tt "three"))
                                        (bridge-time (tt ")")))))))
  
  (exp-time-expr #:c values #:b values ghost)
  (exp-time-expr #:b values ghost)
  (exp-time-expr ghost)
  ;(exp-time-expr values)
  
  (define (drdemo p)
    (demo p 'se values #:find ct-find (t "Check Syntax")))
  
  (define (expt-time-expr2 #:c [compile-time compile-time] #:b [bridge-time bridge-time]
                           #:d [drdemo values] #:g [level gauge:all-lisp])
    (slide/gauge
     #:level level
     #:title expand-time-title
     (code
      (#,(drdemo (code require)) (#,(rbridge-time (code for-syntax)) "roman-numerals.rkt"))
      code:blank
      (define-syntax three
        #, (compile-time 
            (inset-compile
             (code (lambda (stx)
                     #`#,(run-time (code (+ 1 #,(hbl-append (tt "#,") (compile-time (code (roman->number "II"))))))))))))
      code:blank
      (+ 1 (three)))))
  (expt-time-expr2 #:c values #:b values #:g gauge:and-more-lisp)
  (expt-time-expr2 #:b values)
  #;
  (expt-time-expr2 #:b values #:d drdemo)

  (define (expt-time-expr3 #:c [compile-time compile-time] #:b [bridge-time bridge-time]
                           #:g [level gauge:all-lisp])
    (slide/gauge
     #:level level
     #:title expand-time-title
     (code
      (#,(rbridge-time (code begin-for-syntax))
       #,(compile-time
          (code (define (roman->number str) ....))))
      code:blank
      (define-syntax three
        #, (compile-time 
            (inset-compile
             (code (lambda (stx)
                     #`#,(run-time (code (+ 1 #,(hbl-append (tt "#,") (compile-time (code (roman->number "II"))))))))))))
      code:blank
      (+ 1 (three)))))
  
  (expt-time-expr3 #:c values #:b values #:g gauge:and-more-lisp)
  (expt-time-expr3 #:b values))

;; ================================================================================

(define (rep-code-slides)
  ;; ----------------------------------------
  
  (define rep-code-title "Representing Code")
  
  (define (cpara . rest)
    (apply para #:width (- (current-para-width) (* 2 gap-size)) rest))
  
  #;
  (slide/gauge
  #:level gauge:scheme
  #:title rep-code-title
  (para "Code representation isn't just text")
  (blank)
  (cpara
  (code
  (define-syntax three
  (lambda (stx) #'(+ 2 1)))
  code:blank
  (let ([+ -])
  (three))
  (code:comment "produces 3, not 1"))))
  
  #;
  (slide/gauge
  #:level gauge:scheme
  #:title rep-code-title
  (para "Lexical context exists before parsing")
  (blank)
  (cpara
  (code
  (banana (lambda (x) 
  (ananab x)))))
  'next
  (blank)
  (blank)
  (blank)
  (para "... in contrast to Template Haskell")
  (cpara
  (vl-append (current-code-line-sep)
  (tt "$(banana [| \\x =>")
  (tt "             $(ananab [| x |]) |])"))))
  
  #;
  (slide/gauge
   #:level gauge:scheme
   #:title rep-code-title 
   (cpara
    (code
     (class ....
       code:blank
       (lambda (x) 
         ... this ...)
       code:blank
       code:blank)))
   'next
   (blank)
   (para "versus")
   (cpara
    (vl-append (current-code-line-sep)
               (tt "$(class ....")
               (tt " ")
               (tt "    [| \\x =>")
               (tt "         ... $(this) ... |]")
               (tt " ")
               (tt "    )"))))
  
  (define (syntax-object-slide src val pos)
    (slide/gauge
     #:level gauge:all-scheme
     #:title rep-code-title 
     #:layout 'top
     (blank)
     (vc-append
      (* 3 gap-size)
      src
      (syntax-object-record val pos #f))))    
  
  (syntax-object-slide (code #'lambda) (code 'lambda) 13)
  (syntax-object-slide (code #'(lambda (x) x))
                       (let* ([datum (rec-tt "datum")]
                              [r (record
                                  (rec-tt "syntax-object")
                                  datum
                                  (rec-tt "srcloc")
                                  (rec-tt "context"))]
                              [r (let* ([p (refocus r datum)]
                                        [d (/ (- (pict-height r) (pict-height p)) 2)]
                                        [p (inset p (/ gap-size 2) d (- (pict-width r) (pict-width p)) d)])
                                   (scale (use-last p p) 0.7))]
                              [r2 (launder r)]
                              [r3 (launder r)])
                         (code (#,(hc-append gap-size r r2 r3))))
                       12))

;; ----------------------------------------

(module+ main
  (slide pipeline-pict))
