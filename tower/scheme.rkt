#lang slideshow
(require slideshow/code
         slideshow/step
         scheme/class
         scheme/gui/base
         (for-syntax syntax/stx)
         (only-in syntax/parse syntax-parse)
         "utils.ss"
         "scode.rkt"
         "color.rkt")

(provide pattern-slides
         simple-pattern-slides
         pattern-...-slides
         lexical-slides
         lexical-how-slides
         define-syntax-slides
         syntax-case-slides)

;; Unusual indentation needed:
;;   let^1
;;   let-one
;; Watch out for (#,...) in two cases

(define dt bit)

(define lex-level-1 (gauge:back-to-scheme 1/7))
(define lex-level-2 (gauge:back-to-scheme 2/7))
(define lex-level-3 (gauge:back-to-scheme 3/7))
(define lex-level-4 (gauge:back-to-scheme 4/7))
(define lex-level-5 (gauge:back-to-scheme 5/7))
(define lex-level-6 (gauge:back-to-scheme 6/7))
(define lex-level-7 (gauge:back-to-scheme 7/7))

(define swap-defn
  (scode
   (define-syntax-rule (swap a b)
     (let ([tmp b])
       (set! b a)
       (set! a tmp)))))

(define (expands-table . l)
  (table 3
         l
         ltl-superimpose ltl-superimpose
         gap-size (current-line-sep)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simple-pattern-slides)
  (void
   (with-steps
    (dsr pat-tmpl pattern template)
    (slide/gauge
     #:level lex-level-1
     #:title "Simple Pattern-Based Macros"
     (lt-superimpose
      ((vbetween dsr dsr)
       (scode
        (define-syntax-rule .....
          .....)))
      ((vbetween pat-tmpl pat-tmpl)
       (scode
        (define-syntax-rule #,pattern
          #,template)))
      ((vbetween pattern pattern)
       (scode
        (define-syntax-rule (swap a b)
          .....)))
      ((vafter template)
       swap-defn))
     (blank)
     (ct-superimpose
      ((vbetween dsr dsr)
       (item (code define-syntax-rule) "indicates a simple-pattern macro definition"))
      ((vbetween pat-tmpl pat-tmpl)
       (vc-append
        gap-size
        (item "A" pattern "to match")
        (item "Produce result from" template)))
      ((vbetween pattern pattern)
       (vc-append
        gap-size
        (item "Pattern for this macro:" (code (swap a b)))
        (item "Each identifier matches anything in use")
        (expands-table
         (code (swap x y))
         expands
         (para #:fill? #f (code a) (t "is") (code x))
         (blank)(blank)
         (para #:fill? #f (code b) (t "is") (code y))
         (tt " ")(blank)(blank)
         (code (swap 9 (+ 1 7)))
         expands
         (para #:fill? #f (code a) (t "is") (code 9))
         (blank)(blank)
         (para #:fill? #f (code b) (t "is") (code (+ 1 7))))))
      ((vbetween template template)
       (vc-append
        gap-size
        (para #:width longish
              "Bindings substituted into template to generate the result")
        (expands-table
         (code (swap x y))
         expands
         (code (let ([tmp y])
                 (set! y x)
                 (set! x tmp)))
         (tt " ")(blank)(blank)
         (code (swap 9 (+ 1 7)))
         expands
         (code (let ([tmp (+ 1 7)])
                 (set! (+ 1 7) 9)
                 (set! 9 tmp)))))))))))

;; ----------------------------------------

(define (pattern-slides)
  (void
   (with-steps
    (shift ds rs pat-tmpl pattern template)
    (slide/gauge
     #:level lex-level-1
     #:title "Pattern-Based Macros"
     (lt-superimpose
      ((vbetween shift ds)
       (scode
        (define-syntax shift
          .....)))
      ((vbetween rs rs)
       (scode
        (define-syntax shift
          (syntax-rules (back)
            .....))))
      ((vbetween pat-tmpl pat-tmpl)
       (code
        (define-syntax shift
          (syntax-rules (back)
            [#,pattern #,template]
            ...
            [#,pattern #,template]))))
      ((vbetween pattern pattern)
       (scode
        (define-syntax shift
          (syntax-rules (back)
            [(shift a b c) .....]
            [(shift back a b c) .....]))))
      ((vafter template)
       (scode
        (define-syntax shift
          (syntax-rules (back)
            [(shift a b c) (begin
                             (swap a b)
                             (swap b c))]
            [(shift back a b c) (begin
                                  (swap c b)
                                  (swap b a))])))))
     (blank)
     (ct-superimpose
      ((vbetween shift shift)
       (htl-append
        (* 3 gap-size)
        (code
         (let ([x 0]
               [y 1]
               [z 2])
           (shift x y z)))
        (code
         (let ([x 0]
               [y 1]
               [z 2])
           (shift back x y z)))))
      ((vbetween ds ds)
       (item (code define-syntax) "indicates a macro definition"))
      ((vbetween rs rs)
       (vc-append
        gap-size
        (item (code syntax-rules) "means a pattern-matching macro")
        (item (code (back)) "means that" (code back) "is literal in patterns")))
      ((vbetween pat-tmpl pat-tmpl)
       (vc-append
        gap-size
        (item "Any number of" (hbl-append pattern (t "s")) "to match")
        (item "Produce result from" template "of first match")))
      ((vbetween pattern pattern)
       (vc-append
        gap-size
        (para "Two patterns for this macro")
        (item (code (shift x y z)) "matches first pattern")
        (item (code (shift back x y z)) "matches second pattern")))
      ((vbetween template template)
        (expands-table
         (code (shift x y z))
         expands
         (code (begin
                 (swap x y)
                 (swap y z)))
         (tt " ")(blank)(blank)
         (code (shift back x y z))
         expands
         (code (begin
                 (swap z y)
                 (swap y x))))))))))
  
;; ----------------------------------------

(define (lexical-slides)
  (void
   (with-steps
    (setup setupx lexical)
    (slide/gauge
     #:level lex-level-2
     #:title "Macro Scope"
     swap-defn
     (blank)
     (vc-append
      gap-size
      (para #:width longish "What if we" (code swap) "a variable named" (code tmp) "?")
      (expands-table
       (code (let ([tmp 5]
                   [other 6])
               (swap tmp other)))
       (cc-superimpose
        ((vbetween setup setupx)
         (vc-append (- (current-line-sep)) (colorize (t "?") RedColor) expands))
        ((vafter lexical)
         expands))
       (lc-superimpose
        ((if (between? setup setupx) values ghost)
         (code (let ([tmp 5]
                     [other 6])
                 (let ([tmp other])
                   (set! other tmp)
                   (set! tmp tmp)))))
        ((if (between? setup setupx) ghost values)
         (code (let ([tmp 5]
                     [other 6])
                 (let ([tmp_1 other])
                   (set! other tmp)
                   (set! tmp tmp_1))))))))
     (lt-superimpose
      ((vbetween setupx setupx)
       (colorize (para #:fill? #t (it "This expansion would break scope")) RedColor))
      ((vafter lexical)
       (colorize (para #:fill? #t "Rename the introduced binding") BlueColor))))))
  
  (slide/gauge
   #:level lex-level-3
   #:title
   "Macro Scope: Local Bindings"
   (para #:width longish "Macro scope means that local macros work, too:")
   (scode (define (f x)
            (define-syntax swap-with-arg
              (syntax-rules ()
                [(swap-with-arg y) (swap x y)]))
            (code:encloud
             (code:line
              code:blank
              (let ([z 12]
                    [x 10])
                (code:comment "Swaps z with original x:")
                (swap-with-arg z))
              code:blank))))))

;; ----------------------------------------

(define (pattern-...-slides)

  (define seq-title "Matching Sequences")

  (slide
   #:title seq-title
   (para "Some macros need to match sequences")
   (vl-append
    gap-size
    (code (rotate x y))
    (code (rotate red green blue))
    (code (rotate front-left
                  rear-right
                  front-right
                  rear-left))))
  
  (slide
   #:title seq-title
   (scode (define-syntax rotate
            (syntax-rules ()
              [(rotate a) (void)]
              [(rotate a b c ...) (begin
                                    (swap a b)
                                    (rotate b c ...))])))
   (blank)
   (item (code ...) "in a pattern: multiple of previous sub-pattern")
   (table 3
	  (list (code (rotate x y z w))
		expands
		(para #:fill? #f (code c) "is" (code z w)))
	  ltl-superimpose ltl-superimpose
	  gap-size (current-line-sep))
   'next
   (item (code ...) "in a template: multiple instances of previous sub-template")
   (table 3
	  (list (code (rotate x y z w))
		expands
		(code (begin
			(swap x y)
			(rotate y z w))))
	  ltl-superimpose ltl-superimpose
	  gap-size (current-line-sep)))

  (slide
   #:title seq-title
   (scode (define-syntax rotate
            (syntax-rules ()
              [(rotate a c ...) 
               (shift-to (c ... a) (a c ...))]))
          code:blank
          (define-syntax shift-to
            (syntax-rules ()
              [(shift-to (from0 from ...) (to0 to ...)) 
               (let ([tmp from0])
                 (set! to from) ...
                 (set! to0 tmp))])))
   (blank)
   (item (code ...) "maps over same-sized sequences")
   (item (code ...) "duplicates constants paired with sequences")))

;; ----------------------------------------

(define (define-syntax-slides)
  (slide/gauge
   #:level lex-level-4
   #:title
   "Transformer Definitions"
   (para #:width longish
         "In general," (code define-syntax) "binds a transformer procedure:")
   (blank)
   (vl-append
    gap-size
    (scode (define-syntax swap
             (syntax-rules .....)))
    expands
    (scode (define-syntax swap
             (lambda (stx)
               (code:encloud #,(vc-append
                                (para #:fill? #f 
                                      "use syntax-object primitives to")
                                (para #:fill? #f
                                      (t "match") (code stx)
                                      (t "and generate result"))))))))))

;; ----------------------------------------

(define (syntax-case-slides #:syntax-parse? [stx-parse? #f])
  
  (define syntax-case-title "Matching Syntax and Having It, Too")
  
  (slide/gauge
   #:level lex-level-5
   #:title syntax-case-title
   (para #:width longish
         (if stx-parse? (code syntax-parse) (code syntax-case))
         "and" (colorize (tt "#'") keyword-color)
         "combine patterns and computation")
   (vl-append
    gap-size
    (if stx-parse?
        (scode (syntax-parse _stx-expr
                 [_pattern _result-expr]
                 ...
                 [_pattern _result-expr]))
        (scode (syntax-case _stx-expr ()
                 [_pattern _result-expr]
                 ...
                 [_pattern _result-expr])))
    (blank)
    (scode #'template)))
  
  (slide/gauge
   #:level lex-level-5
   #:title syntax-case-title
   (vl-append
    gap-size
    swap-defn
    expands
    (if stx-parse?
        (scode (define-syntax swap
                 (lambda (stx)
                   (syntax-parse stx
                     [(swap_1 a b) #'(let ([tmp b])
                                       (set! b a)
                                       (set! a tmp))]))))
        (scode (define-syntax swap
                 (lambda (stx)
                   (syntax-case stx ()
                     [(swap_1 a b) #'(let ([tmp b])
                                       (set! b a)
                                       (set! a tmp))])))))))
  
  (slide/gauge
   #:level lex-level-5
   #:title syntax-case-title
   (para #:width longish "Check for identifiers before expanding:")
   (if stx-parse?
       (scode (define-syntax swap
                (lambda (stx)
                  (syntax-parse stx
                    [(swap a b) 
                     (if (and (identifier? #'a) 
                              (identifier? #'b))
                         #'(let ([tmp b])
                             (set! b a)
                             (set! a tmp))
                         (raise-syntax-error
                          'swap "needs identifiers" 
                          stx))]))))
       (scode (define-syntax swap
                (lambda (stx)
                  (syntax-case stx ()
                    [(swap a b) 
                     (if (and (identifier? #'a) 
                              (identifier? #'b))
                         #'(let ([tmp b])
                             (set! b a)
                             (set! a tmp))
                         (raise-syntax-error
                          'swap "needs identifiers" 
                          stx))])))))))

;; ----------------------------------------

(define how-it-works-title "How Macro Scope Works")
  
(define (lexical-how-slides)
  
  (define roughly-expands expands)
  
  (with-steps
   (obvious one can cannot explain hygiene reftrans)
   (slide/gauge
    #:level lex-level-6
    #:title how-it-works-title
    (lt-superimpose
     ((vbefore one) swap-defn)
     ((vbetween one explain)
      (scode
       (define-syntax-rule (swap a b)
         (let-one [tmp b]
           (set! b a)
           (set! a tmp)))))
     ((vafter hygiene) swap-defn))
    (blank)
    (ct-superimpose
     ((vbefore hygiene)
      (vc-append
       gap-size
       (cc-superimpose
        ((vonly obvious)
         (para #:width longish 
               "Seems obvious that" (code tmp) "can be renamed..."))
        ((vonly can)
         (para #:width longish "Can rename" (code tmp) ":"))
        ((vafter cannot)
         (para #:width longish (colorize (it "Cannot") RedColor) "rename" (code tmp) ":")))
       (lt-superimpose
        ((vonly can)
         (scode (define-syntax-rule (let-one (x v) body)
                  (let ([x v]) body))))
        ((vafter cannot)
         (scode (define-syntax-rule (let-one (x v) body)
                  (list 'x v body)))))
       (blank)
       ((vafter explain)
        (colorize (para #:align 'center
                        "Track identifier introductions,"
                        "then rename only as binding forms are discovered")
                  BlueColor))))
     ((vafter hygiene)
      (ct-superimpose
       ((vbetween hygiene hygiene)
        (vc-append
         gap-size
         (para #:width longish
               "Tracking avoids capture by introduced variables")
         (expands-table
          (code (let ([tmp 5]
                      [other 6])
                  (swap tmp other)))
          roughly-expands
          (code (let ([tmp 5]
                      [other 6])
                  (let^1 ([tmp^1 other])
                         (set!^1 other tmp)
                         (set!^1 tmp tmp^1)))))
         (para #:fill? #f
               (typeset-code (datum->syntax #f (string->symbol "\240^1")))
               "means introduced by expansion")
         (para #:fill? #f (code tmp^1) "does not capture" (code tmp))))
       ((vbetween reftrans reftrans)
        (vc-append
         gap-size
         (para #:width longish
               "Tracking also avoids capture" (it "of") "introduced variables")
         (expands-table
          (code (let ([set! 5]
                      [let 6])
                  (swap set! let)))
          roughly-expands
          (code (let ([set! 5]
                      [let 6])
                  (let^1 ([tmp^1 let])
                         (set!^1 let set!)
                         (set!^1 set! tmp^1)))))
         (para #:fill? #f (code set!) "does not capture" (code set!^1))
         (para #:fill? #f (code let) "does not capture" (code let^1)))))))))

  (void))

;; ----------------------------------------

(define (lexical-detail-slides)  

  (define-syntax pscale (syntax-rules () [(_ e) (scale/improve-new-text e 0.9 0.9)]))
  
  (define precise-how-it-works-title
    (string-append how-it-works-title " (More Precisely)"))
  
  (define (*bright-subsup show id sub mode -superimpose)
    (hbl-append id (let ([p (text (number->string sub)
                                  `(,mode . ,(current-code-font))
                                  (current-font-size))])
                     (refocus (-superimpose
                               (show 
                                (colorize (filled-rectangle (pict-width p) (* (pict-height p) 2/3))
                                          "pink"))
                               p)
                              p))))
  (define-syntax-rule (bright-sub show id sub)
    (*bright-subsup show (code id) sub 'subscript cb-superimpose))
  (define-syntax-rule (bright-sup show id sup)
    (*bright-subsup show (code id) sup 'superscript ct-superimpose))
  
  (with-steps
   (let added added-more continue last-name summary)
   (slide/gauge
    #:level lex-level-7
    #:title precise-how-it-works-title
    #:layout 'top
    (para
     #:width longish
     (htl-append
      gap-size
      (pscale
       (code (let ([tmp 5]
                   [other 6])
               (swap tmp other))))
      ((vafter added) expands)
      ((vafter added)
       (pscale
        (code (let ([#,(bright-sub (vbetween added added-more) tmp 0) 5]
                    [#,(bright-sub (vbetween added added-more) other 0) 6])
                (swap #,(bright-sub (vbetween added added-more) tmp 0) #,(bright-sub (vbetween added added-more) other 0))))))))
    (blank)
    ((vafter continue) 
     (para
      #:width longish
      (htl-append 
       gap-size
       expands
       (pscale
        (code (let ([tmp_0 5]
                    [other_0 6])
                (#,(bright-sup (vonly continue) let 1) ([#,(bright-sup (vbetween continue last-name) tmp 1) other_0])
                 (#,(bright-sup (vonly continue) set! 1) other_0 tmp_0)
                 (#,(bright-sup (vonly continue) set! 1) tmp_0 #,(bright-sup (vbetween continue last-name) tmp 1))))))
       ((vafter last-name) expands)
       ((vafter last-name)
        (pscale
         (code (let ([tmp_0 5]
                     [other_0 6])
                 (let^1 ([#,(bright-sub (vonly last-name) tmp 2) other_0])
                        (set!^1 other_0 tmp_0)
                        (set!^1 tmp_0 #,(bright-sub (vonly last-name) tmp 2))))))))))
    (blank)
    (blank)
    (lt-superimpose
     ((vbetween added added-more)
      (vc-append
       gap-size
       (para #:width longish
             "To parse" (code let)
             ", rename bindings by adding a subscript")
       ((vbetween added-more added-more)
        (vc-append
         gap-size
         (para #:width longish
               "To parse" (code quote) ", drop the subscript")
         (htl-append gap-size
                     (code (let ([x 1])
                             'x))
                     expands
                     (code (let ([x_1 1])
                             'x_1))
                     expands
                     (code (let ([x_1 1])
                             'x)))))))
     ((vbetween continue continue)
      (para #:width longish
            "Mark superscripts on introduced identifiers"))
     ((vbetween last-name last-name)
      (para #:width longish
            "Rename for" (code let)
            "--- but only where superscript marks match"))
     ((vbetween summary summary)
      (vc-append
       gap-size
       (item "Introductions" (dt "marked") "with fresh superscript")
       (item "Matching marks" (dt "renamed") "with fresh subscript")
       (para #:width longish " "))))))
  
  ;; >>>>>>>> CUT and PASTE from previous <<<<<<
  ;;  The problem is that it's really difficult to abstract
  ;;  when the source location matters....
  (with-steps
   (let added continue last-name)
   (slide/gauge
    #:level lex-level-7
    #:title precise-how-it-works-title
    #:layout 'top
    (para
     #:width longish
     (htl-append
      gap-size
      (pscale
       (code (let ([set! 5]
                   [let 6])
               (swap set! let))))
      ((vafter added) expands)
      ((vafter added)
       (pscale
        (code (let ([#,(bright-sub (vbetween added added) let 0) 5]
                    [#,(bright-sub (vbetween added added) set! 0) 6])
                (swap #,(bright-sub (vbetween added added) let 0) #,(bright-sub (vbetween added added) set! 0))))))))
    (blank)
    ((vafter continue) 
     (para
      #:width longish
      (htl-append 
       gap-size
       expands
       (pscale
        (code (let ([let_0 5]
                    [set_0 6])
                (#,(bright-sup (vonly continue) let 1) ([#,(bright-sup (vbetween continue last-name) tmp 1) set!_0])
                 (#,(bright-sup (vonly continue) set! 1) let_0 set!_0)
                 (#,(bright-sup (vonly continue) set! 1) let_0 #,(bright-sup (vbetween continue last-name) tmp 1))))))
       ((vafter last-name) expands)
       ((vafter last-name)
        (pscale
         (code (let ([set!_0 5]
                     [let_0 6])
                 (let^1 ([#,(bright-sub (vonly last-name) tmp 2) let_0])
                        (set!^1 let_0 set!_0)
                        (set!^1 set!_0 #,(bright-sub (vonly last-name) tmp 2))))))))))
    (blank)
    (blank)
    (lt-superimpose
     ((vbetween last-name last-name)
      (para #:width longish "Superscript mark is" (bt "not") "a rename:"
            (code let^1) "refers to" (code let))))))
  
  (void))

;; ----------------------------------------

(module+ main
  (pattern-slides)
  (simple-pattern-slides)
  (pattern-...-slides)
  (lexical-slides)
  (lexical-how-slides)
  (define-syntax-slides)
  (syntax-case-slides))
