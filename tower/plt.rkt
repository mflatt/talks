#lang slideshow
(require slideshow/code
         slideshow/step
         slideshow/balloon
         scheme/class
         scheme/gui/base
         "utils.rkt")

(provide implicit-slides
         cooperate-slides)

(define (implicit-slides)
  (define implicit-title "Implicit Syntactic Forms")

  (define (sube* e n)
    (hbl-append 2
                e
                (text (format "~a" n) 
                      `(subscript . ,(current-code-font))
                      (current-font-size))))

  (define-syntax-rule (sube e n)
    (sube* (code e) 'n))

  (slide
   #:title implicit-title
   (para #:width WIDE "To change functions:")
   (code (define-syntax-rule (lambda ....) ....))
   'next
   (blank (* 2 gap-size))
   (para #:width WIDE "To change function calls?")
   'next
   (code (define-syntax-rule (#%app ....) ....))
   'next
   (blank)
   (code (#,(sube _expr 1) ... #,(sube _expr N)))
   (para #:align 'center "is implicitly")
   (code (#%app #,(sube _expr 1) ... #,(sube _expr N))))

  (slide
   #:title implicit-title
   (code #,(tt "#lang") s-exp _path
         #,(sube _form 1)
         ...
         #,(sube _form N))
   (blank gap-size)
   (para #:align 'center "is implicitly")
   (blank gap-size)
   (code #,(tt "#lang") s-exp _path
         (#%module-begin
          #,(sube _form 1)
          ...
          #,(sube _form N)))))


(define (cooperate-slides)
  ;; ----------------------------------------
  
  (define (gauge:plt n)
    (gauge:partial-plt-scheme (/ n 6)))
  
  ;; ----------------------------------------

  (define rep-ast-title "Representing an AST")
  
  (slide/gauge
   #:level (gauge:plt 1)
   #:title rep-ast-title
   (para #:align 'center "An AST is represented by a syntax object")
   (blank)
   (blank)
   (hc-append
    gap-size
    (record
     (rec-tt "define")
     (rec-tt "doc")
     (rec-sub
      (record
       (rec-tt "apply")
       (rec-tt "bold")
       (rec-tt "(\"Hi\")"))))
    (tt "=")
    (code (define-values (doc)
            (#%app bold (quote "Hi"))))))

  ;; ----------------------------------------
  
  (define -is- (tt "::="))
  (define -or- (tt " | "))
  
  (define-syntax-rule (grammar [id form1 form ...] ...)
    (table
     3
     (cdddr
      (append
       (append
        (let ([-id (code id)])
          (append
           (list (blank) (blank) (tt " "))
           (list -id -is- (code form1))
           (list (blank) -or- (code form)) ...)))
       ...))
     ltl-superimpose ltl-superimpose
     gap-size (current-code-line-sep)))
  
  
  (define scheme-grammar
    (vl-append
     (* 3 gap-size)
     (ht-append
      (* 3 gap-size)
      (grammar
       [_mod (module id name-id
               (#%plain-module-begin
                _mod-form ...))])
      (grammar
       [_mod-form _expr
                  (#%provide _spec ...)
                  (define-values (_id ...) _expr)
                  (define-syntaxes (_id ...) _expr)
                  (define-values-for-syntax (_id ...)
                    _expr)
                  (#%require raw-require-spec ...)]))
     (ht-append
      (* 3 gap-size)
      (grammar
       [_expr _id
              (#%plain-lambda _formals _expr ...+)
              (case-lambda (_formals _expr ...+) ...)
              (if _expr _expr _expr)
              (begin _expr ...+)
              (begin0 _expr _expr ...)
              (let-values (((_id ...) _expr) ...)
                _expr ...+)
              (letrec-values (((_id ...) _expr) ...)
                _expr ...+)
              (set! _id _expr)
              (#, (code quote) _datum)
              (quote-syntax _datum)
              (with-continuation-mark _expr _expr _expr)
              (#%plain-app _expr ...+)
              (#%top . _id)
              (#%variable-reference . _global)])
      (grammar
       [_formals (_id ...)
                 (_id ...+ . _id)
                 _id]))))
  
  (slide/gauge
   #:level (gauge:plt 1)
   #:title rep-ast-title
   (scale scheme-grammar 0.57))
  
  ;; ----------------------------------------

  (with-steps~
   (->app app-demo ->datum ->quote ->module-begin mb-demo)
   (slide/gauge
    #:level (gauge:plt 2)
    #:title "Implicit Keywords"
    (table
     3
     (list (code (+ 1 2))
           expands
           (let ([p (code (#%app + 1 2))])
             (if (after? app-demo)
                 (demo p 'sw values (t "Lazy Scheme") (t "Beginner Scheme"))
                 p))
           (blank)
           ((vafter ->datum) expands)
           ((vafter ->datum)
            (code (#%app + 
                         (#%datum 1) 
                         (#%datum 2))))
           
           (blank)
           ((vafter ->quote) expands)
           ((vafter ->quote) (code (#%app + (quote 1) (quote 2))))
           
           (tt " ") (blank) (blank)
           (tt " ") (blank) (blank)
           
           ((vafter ->module-begin)
            (code (module m scheme
                    (define x 3)
                    (+ x 1))))
           ((vafter ->module-begin)
            expands)
           (let ([p ((vafter ->module-begin)
                     (code (module m scheme
                             (#%module-begin
                              (define x 3)
                              (+ x 1)))))])
             (if (after? mb-demo)
                 (demo p 'sw values (t "Scheme") (t "Scribble"))
                 p)))
     (cons rtl-superimpose ltl-superimpose)
     (cons rtl-superimpose ltl-superimpose)
     (* 1 gap-size) (* 2 (current-code-line-sep)))))
  
  ;; ----------------------------------------
  
  (define explicit-expansion-title "Explicit Expansion")
  
  (slide/gauge
   #:level (gauge:plt 3)
   #:title explicit-expansion-title
   (para "For tools that manipulate whole programs:")
   (code expand : syntax -> syntax)
   (blank)
   (blank)
   (para "For macro transformers:")
   (code local-expand : syntax .... -> syntax)
   (para #:align 'right "... and extra arguments to say where to stop"))
  
  (define (add-copy p strs)
    (rb-superimpose
     p
     (clickback (colorize
                 (scale
                  (let ([p (t "Copy")])
                    (refocus
                     (vc-append p
                                (linewidth 2 (hline (pict-width p) 0)))
                     p))
                  0.75)
                 "blue")
                (lambda ()
                  (send the-clipboard set-clipboard-string 
                        (string-join strs "\n")
                        0)))))
  
  (with-steps~
   (use defn)
   (slide/gauge
    #:level (gauge:plt 3)
    #:title explicit-expansion-title
    (vl-append
     (* 4 gap-size)
     (add-copy
      (code
       (define-syntax-rule (thunk body)
         (lambda () body))
       code:blank
       (class object%
         (define/public me (thunk this)) code:blank code:blank
         ...))
      '("#lang scheme"
        ""
        "(define-syntax-rule (thunk body)"
        "  (lambda () body))"
        ""
        "(define c%"
        "  (class object%"
        "    (define/public me (thunk this))"
        "    (super-new)))"
        ""
        "(send (new c%) me)"))
     ((vafter defn)
      (code
       (define-syntax class
         (lambda (stx)
           .... (local-expand #'method
                              'expression
                              (list #'lambda ....))
           ....)))))))

  ;; ----------------------------------------

  (define adj (lambda (find dx dy)
                (lambda (a b)
                  (let-values ([(x y) (find a b)])
                    (values (+ x dx) (+ y dy))))))
  
  (define (dups p)
    (refocus (lt-superimpose
              (cellophane (inset (launder p) (* 2 gap-size)) 0.15)
              (cellophane (inset (launder p) gap-size) 0.25)
              p)
             p))

  (define intdef-title "Internal Definition Contexts")
  
  (with-steps~
   (pic ->demo)
   (slide/gauge
    #:level (gauge:plt 4)
    #:title intdef-title
    (let ([body-form (dups (code #'defn-or-expr))]
          [local-expand (code local-expand)]
          [intdef (record (rec-tt "intdef") (rec-tt "...") (rec-tt "..."))]
          [install (t "install definition")]
          [defn (dups (code #'(define id ...)))]
          [expr (dups (code #'expr))]
          [out (blank)]
          [pin-arrow-curve (lambda (p src src-find dest dest-find
                                      #:start-angle [sa #f] #:end-angle [ea #f] #:pull [pull 1/4])
                             (pin-arrow-line (/ gap-size 2) p src src-find dest dest-find
                                             #:under? #t
                                             #:start-angle sa #:end-angle ea
                                             #:start-pull pull #:end-pull pull
                                             #:line-width 2                                              
                                             #:color "purple"))])
      (let* ([p (hc-append
                 (* 0 gap-size)
                 (vc-append
                  (* 2 gap-size)
                  (demo intdef 'se (vafter ->demo) (code define-package))                          
                  install)
                 (vr-append
                  (* 2 gap-size)
                  (vc-append
                   (* 3 gap-size)
                   body-form
                   (blank)
                   (blank)
                   local-expand
                   (blank)
                   (hc-append
                    (* 2 gap-size)
                    defn
                    expr))
                  out))]
             [p (pin-arrow-curve p 
                                 body-form cb-find
                                 local-expand ct-find)]
             [p (pin-arrow-curve p 
                                 intdef (adj rc-find 0 (- gap-size))
                                 local-expand (adj ct-find (- gap-size) 0)
                                 #:start-angle 0 #:end-angle (/ pi -2))]
             [p (pin-arrow-curve p 
                                 local-expand cb-find
                                 defn ct-find
                                 #:start-angle (* pi -1/2)
                                 #:end-angle (* pi -3/4))]
             [p (pin-arrow-curve p 
                                 local-expand cb-find
                                 expr ct-find
                                 #:start-angle (* pi -1/2))]
             [p (pin-arrow-curve p 
                                 defn lb-find
                                 install cb-find
                                 #:start-angle (* pi -1) #:end-angle (* pi 1/2) #:pull 1/2)]
             [p (pin-arrow-curve p 
                                 install ct-find
                                 intdef cb-find)]
             [p (pin-arrow-curve p 
                                 defn cb-find
                                 out (adj lt-find gap-size (* gap-size 3/2))
                                 #:start-angle (* pi -1/2) #:end-angle 0)]
             [p (pin-arrow-curve p 
                                 expr cb-find
                                 out (adj lt-find gap-size (* gap-size 2/2))
                                 #:start-angle (* pi -1/2) #:end-angle 0)])
        p))))
  
  (slide/gauge
   #:level (gauge:plt 4)
   #:title intdef-title
   (para #:align 'right
         (code (define-package p (y)
                 (define x 10)
                 (define y x))))
   (syntax-object-record (code 'x) 8
                         (frame
                          (inset
                           (hbl-append (tt "{ rename ") (code x) (tt " to ") (code x_5) (tt ", ...}"))
                           (/ gap-size 3)))))
  
  ;; ----------------------------------------
  
  (define trans-bind-title "Transformer Bindings")
  
  (with-steps~
   (orig arrow def-pkg open-pkg ->demo)
   (slide/gauge
    #:level (gauge:plt 5)
    #:title trans-bind-title
    (blank)
    (let ([p1 (demo (code p) 'sw (vafter ->demo)
                    #:find rt-find
                    (hbl-append (t "provide a package")))]
          [p2 (code p)])
      (let ([e (code
                (define-package #,p1 ....)
                code:blank
                code:blank
                (open-package #,p2))])
        (if (after? arrow)
            (let-values ([(x1 y1) (rb-find e p1)]
                         [(x2 y2) (rt-find e p2)])
              (pin-over (pin-arrow-line
                         (/ gap-size 2)
                         e
                         p1 rb-find
                         p2 rt-find
                         #:start-angle (* pi -1/4)
                         #:end-angle (* pi -3/4)
                         #:color "blue"
                         #:line-width 2)
                        (+ (/ (+ x1 x2) 2) gap-size) (/ (+ y1 y2) 2)
                        (colorize (bt "?") "blue")))                        
            e)))
    (blank)
    (blank)
    (blank)
    (vl-append
     (* 3 gap-size)
     ((vafter def-pkg)
      (code
       (define-package p ....)
       #,expands
       (define-syntax p (make-package ....))))
     ((vafter open-pkg)
      (code
       (open-package p) 
       #,expands
       .... #,(hbl-append (tt "#,") (code (syntax-local-value #'p))) ....)))))
  
  (slide/gauge
   #:level (gauge:plt 5)
   #:title trans-bind-title
   (para #:width longish "Even syntax pattern variables are implemented with macros")
   (blank)
   (scale
    (code (syntax-case stx ()
            [(form id expr) .... #'id ...])
          code:blank
          #,expands
          code:blank
          (let ([m (match-syntax stx ....)])
            (define-syntax form (make-pattern m 0))
            (define-syntax id (make-pattern m 1))
            (define-syntax expr (make-pattern m 2))
            ....
            .... #,(hbl-append (tt "#,") (code (syntax-local-value #'id))) ....
            ....))
    0.75)
   (blank)
   (blank)
   (para #:width longish
         "Primitives include" (code quote-syntax) "," (code syntax-e) "," (code datum->syntax)))
  
  ;; ----------------------------------------
  
  (slide/gauge
   #:level (gauge:plt 6)
   #:title "Fixed Point"
   (hc-append
    (* 2 gap-size)
    (bitmap "yertle.png")
    (vc-append
     gap-size
     (item "New forms can have their own expanders")
     (inset
      (code (define-match-expander polar
              ....)
            code:blank
            (match v
              [(polar mag theta) ....]))
      0 0 (* 4 gap-size) 0)
     (blank)
     (item "New forms can have compile-time functions")
     (subitem (code module) "and" (code syntax-local-lift-expression))
     (subitem (code set!) "and" (code make-set!-transformer))
     (subitem "..."))))
  
  ;; ----------------------------------------
  
  )

