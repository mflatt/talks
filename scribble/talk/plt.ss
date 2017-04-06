#lang at-exp slideshow
(require slideshow/code
         slideshow/play
         slideshow/flash
         (only-in texpict/mrpict record)
         "expand.ss")

(provide plt-slides
         plt-scribble-content
         plt-at-exp-content)

(define (ptt s) (colorize (tt s) (current-base-color)))
(define (pstr s) (colorize (tt s) (current-literal-color)))
(define (rec-tt s) (inset (scale (tt s) 0.8) 5))
(define (rec-sub r) (inset r 5))

(define (modrec in body out)
  (let ([w (+ (max (pict-width in)
                   (pict-width body)
                   (pict-width out))
              gap-size)])
    (frame
     (vc-append (/ gap-size 4)
                in
                (hline w 0)
                body
                (hline w 0)
                out))))

(define (at p)
  (htl-append (colorize (tt "@") (current-base-color)) p))

(define (braces p)
  (hbl-append (colorize (tt "{") (current-base-color))
              p
              (colorize (tt "}") (current-base-color))))

(define (brackets p)
  (hbl-append (colorize (tt "[") (current-base-color))
              p
              (colorize (tt "]") (current-base-color))))

(define (lt s)
  (colorize (tt s) (current-literal-color)))

(define (prog p) (as-file* p))

(define (tree w h #:mid? [mid? #t])
  (let ([p (blank w h)])
    (pin-line
     (pin-line (if mid?
                   (pin-line p
                             p ct-find
                             p cb-find)
                   p)
               p ct-find
               p rb-find)
     p ct-find
     p lb-find)))

(define dots (let ([p (t "...")])
               (inset p (/ (pict-width p) -2) (/ (pict-height p) -2))))

(define (tree2 w h)
  (vl-append
   (let ([tr (tree w h #:mid? #f)])
     (refocus (vl-append tr dots) tr))
   (inset
    (let ([tr (tree w h)])
      (refocus (vc-append (refocus (vl-append tr dots) tr) dots) tr))
    (/ w 2) 0 0 0)))

(define trans-arrow (colorize (arrow (* 2 gap-size) (* pi -1/4)) "forestgreen"))

(define (diag-append d . l)
  (let loop ([l l])
    (if (null? (cdr l))
        (car l)
        (let ([w (pict-width (car l))])
          (vl-append d (car l) (inset (loop (cdr l)) (- w d) 0 0 0))))))

(define (backing p color)
  (cc-superimpose
   (colorize (filled-rectangle (pict-width p) (pict-height p))
             color)
   p))

(define (bright p)
  (refocus
   (cc-superimpose p
                   (colorize
                    (linewidth 4 (ellipse (+ gap-size (pict-width p))
                                          (+ gap-size (pict-height p))))
                    "purple"))
   p))

(define (bright* p n)
  (cc-superimpose p (cellophane (bright (ghost p)) n)))

(define (fade-pict* n p1 p2)
  (define-values (n1 n2) (split-phase n))
  (let ([p (lt-superimpose (fade-pict #:combine lt-superimpose n1 p1 
                                      (cellophane (bright p2) n2))
                           (ghost (launder p1))
                           (ghost (launder p2)))])
    (cc-superimpose
     (cellophane (colorize (filled-flash (pict-width p) (pict-height p)) "white")
                 (sin (* pi (fast-middle n))))
     p)))

(define (fade-around-pict* n p f)
  (cc-superimpose (fade-around-pict n p f)
                  (launder (ghost (f p)))))

(define scheme-code (code scheme))
(define boolean?-code (code boolean?))
(define p-code (code p))
(define p-code2 (launder p-code))

(define srcdoc-prog
  (scale
   (prog
    (code #,(tt "#lang") at-exp #,scheme-code
          (require scribble/srcdoc)
          code:blank
          (provide/doc
           (proc-doc 
            can-wish? (-> [#,p-code person?] #,boolean?-code)
            #,(hbl-append (at (blank))
                          (braces
                           (hbl-append
                            (lt "Returns ")
                            (at (code scheme))
                            (brackets (code #t))
                            (lt " if ")
                            (at (code scheme))
                            (brackets (code #,p-code2))
                            (lt " is a princess."))))))
          code:blank
          (define (can-wish? p) ....)))
   0.8))

(define (linker from to)
  (lambda (p n)
    (lt-superimpose
     p
     (cellophane
      (pin-arrow-line (/ gap-size 2)
                      (ghost p)
                      from cc-find
                      to cc-find
                      #:color "purple"
                      #:line-width 3)
      n))))
                    
(define scheme-link (linker boolean?-code scheme-code))
(define p-link (linker p-code2 p-code))

(define (sweep n d p)
  (if (zero? n)
      p
      (if (= 1 n)
          (ghost p)
          (let ([g (launder (ghost p))])
            (refocus (lt-superimpose
                      g
                      (inset p (* n 1024 d) 0 0 0))
                     g)))))

(define (enum-item n s)
  (item #:bullet (t (format "~a." n)) #:width (/ client-w 2) s))

(define 1-all-module (enum-item 1 "Everything's a module"))
(define 2-separate (enum-item 2 "Separate read, expand, and run"))
(define 3-compose (enum-item 3 "Extension and composition within the language"))
(define 4-lexical (enum-item 4 "Reflect on scope"))

(define (make-example label-n tree-n ast-n exp-n compose-n atexp1-n atexp2-n atexp3-n
                      ex2-n reflect-n lex1-n lex2-n)
  (define-values (exp1-n exp2-n) (split-phase exp-n))
  (define hi-src
    (code #,(tt "#lang") #,(fade-pict* atexp1-n (code scribble/base) (code at-exp scheme/base))
          #,(at (code (require scribble/bnf)))
          code:blank
          #,(htl-append (at (code nonterm))
                        (braces (lt "id")))))
  (define scribble-mod
    (code
     (module m #,(fade-pict* atexp2-n (code scribble/base) (code scheme/base))
       (require scribble/bnf)
       (nonterm "id"))))
  (define hi-ast
    (modrec
     (code scribble/base
           scribble/bnf)
     (fade-around-pict*
      (- 1 atexp3-n)
      (bright* (code (nonterm "id")) atexp3-n)
      (lambda (g)
        (code (define doc
                #,g))))
     (bright* (cellophane (code doc) (- 1 atexp3-n)) atexp3-n)))
  (let ([s (scale (prog hi-src) 0.75)]
        [h (cellophane (scale (backing hi-ast "lightgray") 0.75) ast-n)])
    (let ([sg1 (ghost s)]
          [hg1 (ghost h)]
          [sg2 (ghost s)]
          [hg2 (ghost h)])
      (let ([p (refocus (ht-append
                         (hc-append sg1
                                    (cellophane (t " = ") 
                                                (* ast-n (- 1 exp1-n))))
                         hg1)
                        sg1)])
        (let ([in-tree
               (refocus (table 2 (list (cellophane
                                        (tree2 (/ (pict-width p) 3) (/ (pict-height p) 5))
                                        (* tree-n (- 1 exp1-n)))
                                       (blank) (blank) 
                                       p)
                               cc-superimpose cc-superimpose 0 0)
                        p)]
              [expand
               (cellophane
                (diag-append
                 gap-size
                 sg2
                 (refocus (hc-append gap-size (inset (colorize (it "read") "blue") 0 0 0 6) trans-arrow) trans-arrow)
                 (frame (backing (inset (scale scribble-mod 0.75) 5) "lightblue"))
                 (refocus (hc-append (* gap-size 0.75) trans-arrow (inset (colorize (it "expand") "blue") 0 0 0 6)) trans-arrow)
                 hg2)
                exp2-n)])
          (let ([p (cc-superimpose
                    in-tree
                    expand)])
            (rt-superimpose
             (inset
              (fade-pict
               #:combine lt-superimpose
               reflect-n
               (fade-pict
                #:combine lt-superimpose
                compose-n
                (fade-pict
                 exp-n
                 (cellophane 1-all-module label-n)
                 2-separate)
                3-compose)
               4-lexical)
              (* 2 gap-size) (* 4 gap-size))
             (cc-superimpose
              (sweep
               ex2-n -1
               (slide-pict
                (slide-pict
                 p s
                 sg1 sg2
                 exp1-n)
                h
                hg1 hg2
                exp1-n))
              (sweep (- 1 ex2-n) 1 (p-link (scheme-link srcdoc-prog lex1-n) lex2-n))
              full-page))))))))

(define (plt-slides #:scope? [scope? #t])
  (play-n #:name "PLT"
          (if scope?
              make-example
              (lambda (label-n tree-n ast-n exp-n compose-n atexp1-n atexp2-n atexp3-n)
                (make-example label-n tree-n ast-n exp-n compose-n atexp1-n atexp2-n atexp3-n
                              0 0 0 0)))))

(define (plt-scribble-content)
  (make-example 1 1 1 1 1 0 0 0 0 0 0 0))

(define (plt-at-exp-content)
  (make-example 1 1 1 1 1 1 1 1 0 0 0 0))
