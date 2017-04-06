#lang slideshow
(require "code-sequence.rkt"
         slideshow/balloon
         slideshow/play
         (only-in racket/draw make-color)
         (only-in "code.rkt" encolors scope1 scope2 scope3)
         (only-in racket/gui/base the-clipboard))

(provide pico-slides)

(set-spotlight-style! #:color (make-color 200 0 255 0.5))

(define a-scope-example
  (scope-example (code (let ([#,(encolors (code a) scope1) '1])
                         (let ([#,(encolors (code z) scope1 scope2) '2])
                           ....)))))

(define a-simple-scope-example
  (scope-example (code (lambda (#,(encolors (code a) scope1))
                         ....))))
(define a-macro-scope-example
  (scope-example (code (let-syntax ([#,(encolors (code a) scope1) _macro-expr])
                         ....))))

(define b-scope-example
  (scope-example (code (let ([#,(encolors (code b) scope1) '1])
                         (let ([#,(encolors (code b) scope1 scope2) '2])
                           ....)))))

(define c-scope-example
  (scope-example (code (list
                        (let ([#,(encolors (code c) scope1) '1]) ...)
                        (let ([#,(encolors (code c) scope2) '2]) ...)))))

(define (make-let-syntax-example [apply-macro? #t] #:pad? [pad? #t] #:stx [stx (code stx)])
  (define c (inset (code (let-syntax ([one (lambda (#,stx)
                                             (quote-syntax '1))])
                           #,(if apply-macro? (code (one)) (code one))))
                   0 0 (* 2 (pict-width (tt " "))) 0))
  (inset (scope-example c #:padding gap-size)
         ;; HACK: right padding preserves alignment through various slide changes
         0 0 (if pad? (* 5 (pict-width (tt " "))) 0) 0))

(define let-syntax-example (make-let-syntax-example))

(define (make-thunk-example body #:stx [stx (code stx)])
  (code (let-syntax ([thunk (lambda (#,stx)
                              (list (quote-syntax lambda)
                                    (list (quote-syntax x))
                                    (second stx)))])
          (thunk #,body))))

(define thunk-example (make-thunk-example (code x)))

(define app-balloon (scope-balloon (code (f '1))))
(define macro-app-balloon (scope-balloon (code (one))))
(define primitive-form-balloon (scope-balloon (code (lambda (x) x))))

(define local-binding-text (t "must be a local binding"))
(define local-binding-balloon (scope-balloon local-binding-text #:code? #f #:spike 'w #:spike-y* 0))

(define x/sc1 (encolors (code x) scope1))
(define y/sc1 (encolors (code y) scope1))

(define (core-forms-balloon #:all? all?)
  (let* ([c (vc-append
             (current-line-sep)
             (t "core")
             (t "forms"))]
         [ex (lambda (b) (refocus (balloon-pict b) c))]
         [p c]
         [p (ex (scope-balloon p #:code? #f #:spike 'e #:spike-y* -1.2))]
         [p (if all?
                (ex (scope-balloon p #:code? #f #:spike 'e #:spike-y* 0.4))
                p)]
         [p (if all?
                (ex (scope-balloon p #:code? #f #:spike 'e #:spike-y* 1.0))
                p)])
    (scope-balloon p #:code? #f #:spike 'e #:spike-y* -0.4)))

;; hacks to support escapes in #:code-balloon { ... }
(define x (blank)) 
(define two (blank))

(define (expands-to result)
  (vc-append
   (* 2 gap-size)
   (para "expands to")
   (para result)))

(define (pin-example p #:space [y-space 0] #:right? [right? #f])
  (define pt (blank (current-para-width) 0))
  (inset
   (pin-over pt
             pt (lambda (p1 p2) (values (if right?
                                       (- (+ (current-para-width)
                                             (/ (- client-w (current-para-width)) 2))
                                          (pict-width p))
                                       0)
                                   (- (pict-height p))))
             p)
   0 y-space 0 0))

(define (scope-example+ p) (scope-example (inset p gap-size) #:square? #f))

(define (out-of-context-example [fade values])
  (code #,(fade (code (lambda (#,(encolors (code x) scope1))
                        ....)))
        #,(encolors (code x) scope1)))

(define (environment-motivation-slides)
  (define (in-big-cloud p)
    (inset (scope-example (inset p (* 2 gap-size) (* 2 gap-size)))
           (* 2 gap-size)
           (* 4 gap-size)))
  (slide
   #:title "Expander and Bindings"
   'alts
   (list
    (list
     (para "Binding table helps the expander connect" (it "use") "to" (it "binding"))
     'alts
     (let* ([bind (encolors (code x) scope1)]
            [ref (launder bind)]
            [p (in-big-cloud (code (lambda (#,bind)
                                     ... #,ref ...)))])
       (list (list p)
             (list (let* ([r (launder bind)]
                          [resolve (code resolve)]
                          [p (pin-over p
                                       p (lambda (p1 p2)
                                           (values (+ gap-size (pict-width p1))
                                                   (* 2 gap-size)))
                                       (code (#,resolve #,r)))]
                          [p (pin-arrow-line (/ gap-size 2)
                                             p
                                             ref rc-find
                                             r cb-find
                                             #:start-angle 0
                                             #:end-angle (/ pi 2)
                                             #:color "purple"
                                             #:line-width 3)]
                          [p (pin-arrow-line (/ gap-size 2)
                                             p
                                             resolve ct-find
                                             bind ct-find
                                             #:start-angle (/ pi 2)
                                             #:end-angle (/ pi -2)
                                             #:color "purple"
                                             #:line-width 3)])
                     p)))))
    (list
     (para "Expander must still check whether a" (it "use") "makes sense")
     'alts
     (list
      (list
       (in-big-cloud
        (out-of-context-example)))
      (list
       (inset
        (in-big-cloud
         (code (let-syntax ([#,(encolors (code m) scope1) (lambda (stx) ...)])
                 ....)
               #,(encolors (code m) scope1)))
        0 0 0 (* -2 gap-size))))
     'next
     (vc-append
      (current-line-sep)
      (para #:width (* client-w 0.9) (bt "Compile-time environment") "maps a binding to either")
      (item "the constant" (code variable))
      (item "a macro-transformer function"))))))

;; --------------------------------------------------------------------------------

(define (nt s) (t (format "⟨~a⟩" s)))
(define dt it)

(define grammar-table
   (table
    4
    (list (nt "expr") (t "::=") (code (lambda (#,(nt "id")) #,(nt "expr")))     (dt "function")
          (blank)     (t "|")   (nt "id")                                  (dt "variable")
          (blank)     (t "|")   (code (#,(nt "expr") #,(nt "expr") ...))   (dt "function call")
          (blank)     (t "|")   (code (QUOTE #,(nt "datum")))              (dt "literal data")
          (blank)     (t "|")   (code (let-syntax ([#,(nt "id") #,(nt "expr")])
                                        #,(nt "expr")))                    (dt "macro binding")
          (blank)     (t "|")   (code (quote-syntax #,(nt "datum")))       (dt "literal syntax"))
    ltl-superimpose ltl-superimpose
    gap-size (* 2 (current-line-sep))))

(define (grammar-content #:grammar-i [grammar-i 1.0]
                         #:example [example #f]
                         #:expand? [expand? #f]
                         #:argument? [argument? expand?])
  (define gt (inset grammar-table 0 (* 1 gap-size) 0 0))
  (define gt1 (ghost gt))
  (define gt2 (ghost gt))
  (define p
    (cc-superimpose
     gt1
     (vc-append
      gap-size
      gt2
      (ct-superimpose
       ((if (equal? example 1) values ghost)
        (vl-append
         gap-size
         (blank gap-size)
         (let ([stx (code stx)])
           (define p (scope-example (make-let-syntax-example #:pad? #f #:stx stx)))
           (if argument?
               (pin-balloon (scope-balloon (code (one))
                                           #:spike 's
                                           #:alt-color? #t)
                            p
                            stx ct-find)
               p))
         ((if expand? values ghost)
          (hc-append (* 2 gap-size)
                     (t "expands to")
                     (inset (scope-example (code '1) #:square? #f #:end-margin 0)
                            0 (* -1/2 gap-size) 0 0)))))
       ((if (equal? example 2) values ghost)
        (vl-append
         gap-size
         (blank gap-size)
         (let ([stx (code stx)])
           (define p (inset (scope-example (inset (scale (make-thunk-example (code '1) #:stx stx) 0.8)
                                                  (/ gap-size 2)))
                            (* -1/2 gap-size)))
           (if argument?
               (pin-balloon (scope-balloon (code (thunk '1))
                                           #:spike 's
                                           #:alt-color? #t)
                            p
                            stx ct-find)
               p))
         ((if expand? values ghost)
          (hc-append (* 2 gap-size)
                     (t "expands to")
                     (scope-example (code (lambda (x) '1)) #:end-margin 0)))))))))
  (define gt3 (ghost gt))
  (pin-under (slide-pict p gt3 gt1 gt2 (fast-start grammar-i))
             gt3 lt-find
             gt))

(define (grammar-slides)
  (define title "A Small Language with Hygienic Macros")
  (play-n
   #:name title
   #:skip-last? #t
   (lambda (n)
     (grammar-content #:grammar-i n)))
  (slide #:name title (grammar-content #:example 1))
  (slide #:name title (grammar-content #:example 1 #:argument? #t))
  (slide #:name title (grammar-content #:example 1 #:expand? #t))
  (slide #:name title (grammar-content #:example 2))
  (slide #:name title (grammar-content #:example 2 #:argument? #t))
  (slide #:name title (grammar-content #:example 2 #:expand? #t)))

;; --------------------------------------------------------------------------------

(define (pico-slides)
  (code-sequence
   
   ;; ----------------------------------------
   
   #:external ,grammar-slides


   ;; ============================================================
   #:part "Part 1 - Representing Syntax"

   ;; ----------------------------------------
   #:section "Syntax Objects"
   
   #:code
   (code:comment "Combine a symbol with a set of scopes")
   (struct syntax (e scopes) #:transparent)
   
   #:example-def
   #,(inset (para #:align 'center (scope-example+ (code x)))
            gap-size)
   
   #:example
   code:blank #:!
   (syntax 'x (set)) #:!

   #:example-def
   #,(inset (para #:align 'center (scope-example+ (encolors (code x) scope1)))
            gap-size)
   
   #:example
   (syntax 'x (set sc1)) #:!

   #:example-def
   #,(inset (para #:align 'center (scope-example+ (encolors (code x) scope1 scope2)))
            gap-size)
   
   #:example
   (syntax 'x (set sc1 sc2)) #:!

   #:example-def
   
   #:example
   (syntax? (syntax 'x (set))) #:=> #t
   (syntax? 'x) #:=> #f
   (syntax-e (syntax 'x (set))) #:=> 'x
   (syntax-scopes (syntax 'x (set))) #:=> (set)
   
   #:code
   (code:comment "All syntax object are identifiers")
   (define (identifier? s)
     (syntax? s))
   
   #:example
   (identifier? (syntax 'x (set))) #:=> #t
   
   #:code
   (code:comment "`datum->syntax` coerces to syntax with no scopes")
   (code:comment "leaving existing syntax as-is")
   (define (datum->syntax v)
     (cond
      [(syntax? v) v]
      [(symbol? v) (syntax v (set))]
      [(list? v) (map datum->syntax v)]
      [else v]))
   
   #:example
   (datum->syntax 'a) #:=> (syntax 'a (set))
   (datum->syntax 1) #:=> 1
   (datum->syntax '(a b c)) #:=> (list (syntax 'a (set))
                                       (syntax 'b (set))
                                       (syntax 'c (set)))
   (datum->syntax (list 'a
                        (syntax 'b (set sc1))
                        'c))
   #:=> (list (syntax 'a (set))
              (syntax 'b (set sc1))
              (syntax 'c (set)))
   
   #:code
   (code:comment "`syntax->datum` discards scopes")
   (code:comment "produces a plain S-expression")
   (define (syntax->datum s)
     (cond
      [(syntax? s) (syntax-e s)]
      [(list? s) (map syntax->datum s)]
      [else s]))

   #:example
   (syntax->datum (datum->syntax '(a b c))) #:=> '(a b c)
   
   ;; ----------------------------------------
   #:section "Scopes"

   #:code
   (code:comment "Scope is an empty record")
   (code:comment "identity is based on `eq?`")
   (struct scope ())
   
   #:eval (define sc1 (scope))
   #:eval (define sc2 (scope))
   
   #:example-def
   #,(inset (hbl-append gap-size (code (define sc1 (scope))) (encolors (tt " ") scope1))
            0 0 0 (/ gap-size 2))
   #,(hbl-append gap-size (code (define sc2 (scope))) (encolors (tt " ") scope2))
   code:blank
   
   #:example
   (eq? sc1 sc2) #:=> #f
   (eq? sc1 sc1) #:=> #t

   #,(hbl-append gap-size (code (set sc1 sc2)) (encolors (tt " ") scope1 scope2)) #:!
   
   #:temporary-code
   (code:comment "Add a scope everywhere, including in nested")
   (define (add-scope s sc)
     (cond
      [(syntax? s)
       (syntax (syntax-e s)
               (set-add (syntax-scopes s) sc))]
      [(list? s)
       (map (lambda (e) (add-scope e sc)) s)]
      [else s]))
   
   #:example-def
   #,(para #:align 'center 
           (inset (scope-example+ (code x)) gap-size 0)
           "+" (encolors (tt " ") scope1)
           (tt "⇒")
           (inset (scope-example+ (encolors (code x) scope1)) gap-size 0))
   
   #:example
   (add-scope (syntax 'x (set)) sc1) #:=> (syntax 'x (set sc1))
   
   #:example-def
   #:example
   (add-scope (datum->syntax '(x (y))) sc1)
   #:=> (list (syntax 'x (set sc1))
              (list (syntax 'y (set sc1))))
   
   #:example-def
   #:example
   code:blank #:!

   #:code
   (code:comment "Adjust a scope everywhere, including in nested")
   (define (adjust-scope s sc op)
     (cond
      [(syntax? s)
       (syntax (syntax-e s)
               (op (syntax-scopes s) sc))]
      [(list? s)
       (map (lambda (e) (adjust-scope e sc op)) s)]
      [else s]))
   
   #:code
   (define (add-scope s sc)
     (adjust-scope s sc set-add))
   code:blank
   (define (flip-scope s sc)
     (adjust-scope s sc set-flip))
   code:blank
   (define (set-flip s e)
     (if (set-member? s e)
         (set-remove s e)
         (set-add s e)))
   
   #:example
   (add-scope (syntax 'x (set sc1))
              sc2)
   #:=> (syntax 'x (set sc1 sc2))
   (add-scope (syntax 'x (set sc1))
              sc1)
   #:=> (syntax 'x (set sc1))
   
   (flip-scope (syntax 'x (set sc1)) 
               sc2)
   #:=> (syntax 'x (set sc1 sc2))
   (flip-scope (syntax 'x (set sc1 sc2))
               sc2)
   #:=> (syntax 'x (set sc1))
   
   
   ;; ----------------------------------------
   #:section "Global Binding Table"

   #:code
   (code:comment "A binding is either")
   (code:comment " * symbol = core form or primitive")
   (code:comment " * gensym = local binding")
   (define all-bindings (make-hash))
   code:blank
   (define (add-binding! id binding)
     (hash-set! all-bindings id binding))
   
   #:eval (define loc/a (gensym))
   #:example-def
   #,(htl-append
      gap-size
      a-scope-example
      (code (define loc/a (gensym))))
   
   #:example
   (add-binding! (syntax 'a (set sc1)) loc/a) #:!

   #:eval (define loc/b-out (gensym))
   #:eval (define loc/b-in (gensym))
   #:example-def
   #,(htl-append
      gap-size
      b-scope-example
      (code (define loc/b-out (gensym))
            (define loc/b-in (gensym))))
   
   #:example
   (add-binding! (syntax 'b (set sc1)) loc/b-out) #:+
   (add-binding! (syntax 'b (set sc1 sc2)) loc/b-in) #:!
   
   #:eval (define loc/c1 (gensym))
   #:eval (define loc/c2 (gensym))
   #:example-def
   #,(htl-append
      gap-size
      c-scope-example
      (code
       code:blank
       (define loc/c1 (gensym))
       (define loc/c2 (gensym))))

   #:example
   (add-binding! (syntax 'c (set sc1)) loc/c1) #:+
   (add-binding! (syntax 'c (set sc2)) loc/c2) #:!
   
   #:code
   (code:comment "`resolve` finds the binding for an identifier")
   (define (resolve id)
     (define candidate-ids
       (find-all-matching-bindings id))
     (cond
      [(empty? candidate-ids) #f]
      [else
       (define max-id
         (argmax (compose set-count syntax-scopes)
                 candidate-ids))
       (check-unambiguous max-id candidate-ids)
       (hash-ref all-bindings max-id)]))
   
   #:example-def
   #,(pin-example a-scope-example)

   #:example
   (resolve (syntax 'a (set sc1))) #:=> loc/a
   (resolve (syntax 'a (set sc1 sc2))) #:=> loc/a
   (resolve (syntax 'a (set sc2))) #:=> #f

   #:example-def
   #,(pin-example b-scope-example)

   #:example
   (resolve (syntax 'b (set sc1))) #:=> loc/b-out
   (resolve (syntax 'b (set sc1 sc2))) #:=> loc/b-in
   (resolve (syntax 'b (set sc2))) #:=> #f

   #:example-def
   #,(pin-example c-scope-example)

   #:example
   (resolve (syntax 'c (set sc1))) #:=> loc/c1
   (resolve (syntax 'c (set sc2))) #:=> loc/c2
   (resolve (syntax 'c (set sc1 sc2))) #:fail "ambiguous"

   #:example-def
   #:example
   code:blank #:!

   #:code
   (code:comment "Helper: find candidates as bindings with a subset of scopes")
   (define (find-all-matching-bindings id)
     (for/list ([c-id (in-hash-keys all-bindings)]
                #:when (and (eq? (syntax-e c-id) (syntax-e id))
                            (subset? (syntax-scopes c-id)
                                     (syntax-scopes id))))
       c-id))
 
   #:example-def
   #,(pin-example a-scope-example #:space (* 2 gap-size))
   
   #:example
   (find-all-matching-bindings
    (syntax 'a (set sc1)))
   #:=> (list (syntax 'a (set sc1)))
   
   (find-all-matching-bindings
    (syntax 'a (set sc2)))
   #:=> (list)

   (find-all-matching-bindings
    (syntax 'a (set sc1 sc2)))
   #:=> (list (syntax 'a (set sc1)))

   #:example-def
   #,(pin-example b-scope-example #:space (* 2 gap-size))
   
   #:example
   (list->set
    (find-all-matching-bindings
     (syntax 'b (set sc1 sc2))))
   #:=> (set (syntax 'b (set sc1))
             (syntax 'b (set sc1 sc2)))
   
   #:example-def
   #,(pin-example c-scope-example #:space (* 2 gap-size))
   
   #:example
   (list->set
    (find-all-matching-bindings
     (syntax 'c (set sc1 sc2))))
   #:=> (set (syntax 'c (set sc1))
             (syntax 'c (set sc2)))
   
   #:code
   (code:comment "Helper: check that max has a superset for each candidate")
   (define (check-unambiguous max-id candidate-ids)
     (for ([c-id (in-list candidate-ids)])
       (unless (subset? (syntax-scopes c-id)
                        (syntax-scopes max-id))
         (error "ambiguous:" max-id))))
   
   #:example
   (check-unambiguous
    (syntax 'b (set sc1 sc2))
    (list (syntax 'b (set sc1))
          (syntax 'b (set sc1 sc2))))
   #:!
   
   (check-unambiguous
    (syntax 'c (set sc2))
    (list (syntax 'c (set sc1))
          (syntax 'c (set sc2))))
   #:fail "ambiguous"

   ;; ----------------------------------------
   #:section "Core Forms and Primitives"

   #:code
   (code:comment "All core bindings in `core-scope`")
   (define core-scope (scope))
   code:blank
   (define core-forms
     (set 'lambda 'let-syntax 'quote 'quote-syntax))
   (define core-primitives
     (set 'datum->syntax 'syntax->datum 'syntax-e
          'list 'cons 'first 'second 'rest 'map))
   code:blank
   (for ([sym (set-union core-forms core-primitives)])
     (add-binding! (syntax sym (set core-scope)) sym))

   #:example
   (resolve (datum->syntax 'lambda)) #:=> #f
   (resolve (add-scope (datum->syntax 'lambda)
                       core-scope))
   #:=> 'lambda
   (resolve (add-scope (datum->syntax 'cons)
                       core-scope))
   #:=> 'cons

   ;; ----------------------------------------
   #:section "Importing Core Bindings"

   #:code
   (define (introduce s)
     (add-scope s core-scope))
   
   #:example
   (introduce
    (datum->syntax 'cons))
   #:=> (syntax 'cons (set core-scope))
   
   ;; ============================================================
   #:part "Part 2 - Expander Dispatch"
   
   ;; ----------------------------------------
   #:section "Expanding Macros"

   #:code

   #:example-def
   #,let-syntax-example
   #:next
   #:example/left
   #,(expands-to (scope-example (code '1) #:square? #f)) #:!
   
   #:example-def
   #,let-syntax-example
   code:blank
   (define one-prog
     (introduce
      (datum->syntax
       '(let-syntax ([one (lambda (stx)
                            (quote-syntax '1))])
         (one)))))
   code:blank
   #:next
   
   #:example/left
   (syntax->datum (expand one-prog))
   #:=> '(QQUOTE 1)
   
   #:example/left
   (expand one-prog)
   #:=>! (list (syntax 'quote ....core-scope....) 1)

   #:eval-example
   (syntax->datum
    (expand (introduce
             (datum->syntax
              '(let-syntax ([thunk (lambda (stx)
                                     (list (quote-syntax lambda)
                                           (list (quote-syntax x))
                                           (second stx)))])
                (thunk '1))))))
   '(lambda (x) '1)

   ;; ----------------------------------------
   #:section "Expanding Function Calls"
   
   #:example-def
   #,(scope-example (inset (code (list (one) '2)) gap-size 0)  #:square? #t #:wide? #t)
   #,(expands-to (scope-example (inset (code (list '1 '2)) gap-size 0)  #:square? #t #:wide? #t))
   code:blank
   #:next
   #:example
   (expand (introduce
            (datum->syntax '(list '1 '2))))
   #:=> (list
         (syntax 'list (set core-scope))
         (list (syntax 'quote (set core-scope)) 1)
         (list (syntax 'quote (set core-scope)) 2))
   
   ;; ----------------------------------------
   #:section "Expanding Binding Forms"
   
   #:example-def
   #,(scope-example (inset (code (lambda (x) x)) gap-size 0))
   #,(expands-to (scope-example (inset (code (lambda (#,x/sc1) #,x/sc1)) gap-size 0)))
   #,(code code:blank)
   #:next
   #:example
   (expand (introduce
            (datum->syntax '(lambda (x) x))))
   #:=>! (list
          (syntax 'lambda (set core-scope))
          (list (syntax 'x (set core-scope sc1)))
          (syntax 'x (set core-scope sc1)))
   
   ;; ----------------------------------------
   
   #:external ,environment-motivation-slides

   ;; ----------------------------------------
   #:section "Compile-time Environment"

   #:code
   (define empty-env (hash))
   code:blank
   (define (env-extend env key val)
     (hash-set env key val))
   code:blank
   (define (env-lookup env binding)
     (hash-ref env binding #f))
   code:blank
   (define variable (gensym 'variable))
   
   #:example
   (env-lookup empty-env loc/a) #:=> #f
   
   #:example-def
   #,(pin-example a-simple-scope-example #:space gap-size)
   #:example
   (env-lookup
    (env-extend empty-env loc/a variable)
    loc/a)
   #:=> variable
   
   #:eval (define _macro-function (lambda (stx) #f))
   
   #:example-def
   #,(inset (pin-example a-macro-scope-example #:space (* 3 gap-size))
            0 (* -2 gap-size) 0 0)
   #:example
   (env-lookup
    (env-extend empty-env loc/a _macro-function)
    loc/a)
   #:=> _macro-function
   
   ;; ----------------------------------------
   #:section "Expansion Dispatch"

   #:code
   (code:comment "Main `expand` function")
   (define (expand s [env empty-env])
     (cond
      [(identifier? s)
       (code:comment "an identifier by itself")
       (expand-identifier s env)]
      [(and (pair? s)
            (identifier? (first s)))
       (code:comment "\"application\" of an identifier; maybe a form")
       (expand-id-application-form s env)]
      [(list? s)
       (code:comment "application of non-identifier")
       (expand-app s env)]
      [else
       (code:comment "anything else: error")
       (error "bad syntax:" s)]))
   
   #:code-balloon
   {3 19 ,(scope-balloon (encolors (code x) scope1 scope2) #:square? #f)}
   #:code-balloon
   {7 31 ,app-balloon}
   #:code-balloon
   {7 31 ,macro-app-balloon}
   #:code-balloon
   {7 31 ,primitive-form-balloon}
   #:code-balloon
   {10 13 ,(scope-balloon (code ((curried '1) '2)))}
   #:code-balloon
   {13 8 ,(scope-balloon (code 1) #:square? #f)}
   #:next

   #:code
   #:scale 0.95
   (code:comment "Expand an identifier by itself")
   (define (expand-identifier s env)
     (define binding (resolve s))
     (cond
      [(not binding) (error "free variable:" s)]
      [(set-member? core-primitives binding) s]
      [(set-member? core-forms binding) (error "bad syntax:" s)]
      [else
       (define v (env-lookup env binding))
       (cond
        [(eq? v variable) s]
        [(not v) (error "out of context:" s)]
        [else (error "bad syntax:" s)])]))

   #:code-balloon
   {1 28 ,(scope-balloon (code x))}
   #:code-balloon
   {4 17 ,(scope-balloon (code conz) #:spike-y* 1.5)}
   #:code-balloon
   {5 41 ,(scope-balloon (code cons) #:spike-y* 1.5)}
   #:code-balloon
   {6 36 ,(scope-balloon (code lambda) #:spike-y* 1.5)}
   #:code-balloon
   {8 37 ,local-binding-balloon}
   #:code-balloon
   {10 22 ,(scope-balloon (let* ([x (encolors (code x) scope1 scope2)]
                                 [b (code (lambda (#,(launder x))
                                            #,(ghost x)))])
                            (pin-over (cellophane b 0.5)
                                      x lt-find
                                      x))
                          #:square? #t
                          #:spike-y* 1.5)}
   #:code-balloon
   {11 21 ,(scope-balloon (out-of-context-example (lambda (p) (cellophane p 0.5)))
                          #:square? #t #:spike-y* 1.5)}
   #:code-balloon
   {12 10 ,(scope-balloon (code one) #:spike-y* 1.5)}
   #:next

   #:code
   (code:comment "Expand an identifier in \"application\" position")
   (define (expand-id-application-form s env)
     (define binding (resolve (first s)))
     (case binding
       [(lambda) (expand-lambda s env)]
       [(let-syntax) (expand-let-syntax s env)]
       [(quote) s]
       [(quote-syntax) s]
       [else
        (define v (env-lookup env binding))
        (cond
         [(procedure? v)
          (code:comment "apply a macro, then recur")
          (expand (apply-transformer v s) env)]
         [else
          (code:comment "anything else: a function call")
          (expand-app s env)])]))

   #:code-balloon
   {1 37 ,app-balloon}
   #:code-balloon
   {1 37 ,macro-app-balloon}
   #:code-balloon
   {1 37 ,primitive-form-balloon}
   #:code-balloon
   {5 3 ,(core-forms-balloon #:all? #t)}
   #:code-balloon
   {5 3 ,(core-forms-balloon #:all? #f)}
   #:code-balloon
   {6 14 ,(scope-balloon (code 's))}
   #:code-balloon
   {7 21 ,(scope-balloon (code (quote-syntax #,(encolors (code x) scope1 scope2))))}
   #:code-balloon
   {9 34 ,(scope-balloon local-binding-text #:code? #f #:spike 's)}
   #:code-balloon
   {11 21 ,macro-app-balloon}
   #:code-balloon
   {14 11 ,app-balloon}
   #:next

   ;; ----------------------------------------
   #:section "Applying a Macro"

   #:code
   (code:comment "Apply a macro transformer to syntax")
   (define (apply-transformer t s)
     (code:comment "Create a scope to represent the macro step")
     (define intro-scope (scope))
     (code:comment "Tentatively add the scope to the input")
     (define intro-s (add-scope s intro-scope))
     (code:comment "Call the transformer")
     (define transformed-s (t intro-s))
     (code:comment "Flip intro scope to get final result")
     (flip-scope transformed-s intro-scope))
   
   #:example-def
   #,(scope-example (make-let-syntax-example #:pad? #f))
   #:example
   code:blank #:!
   
   #:code-balloon
   {1 30 ,(scope-balloon (code (one)))}
   #:code-balloon+
   {1 27.5 ,(scope-balloon (hbl-append (t "procedure that returns")
                                       (tt "  ")
                                       (scope-example (code '1) #:square? #f #:end-margin 0))
                           #:spike 'se
                           #:code? #f)}
   #:code-balloon/-
   {1 27.5 ,(scope-balloon (hbl-append (t "procedure that returns")
                                       (tt "  ")
                                       (scope-example (code (QUOTE 1)) #:end-margin 0))
                           #:spike 'se
                           #:code? #f)}
   #:code-balloon+
   {7 26 ,(scope-balloon (t "call procedure") #:spike 's #:code? #f #:spike-y* 0.75)}
   #:code-balloon/-
   {7 20 ,(scope-balloon (code (QUOTE 1)) #:spike 's)}
   #:code-balloon+
   {9 40 ,(scope-balloon (code (#,(encolors (code quote) scope3) 1)))}
   #:code-balloon/+-
   {3 30 ,(scope-balloon (encolors (tt " ") scope3) #:code? #f #:spike 'w)}
   
   #:example-def
   #,(scope-example (inset (scale thunk-example
                                  0.8)
                           (/ gap-size 2)))
   #:example
   code:blank #:!
   
   #:code-balloon
   {1 30 ,(scope-balloon (code (thunk x)))}
   #:code-balloon+
   {1 27.5 ,(scope-balloon (hbl-append (t "procedure that expands ")
                                       (code thunk))
                           #:spike 'se
                           #:code? #f)}
   #:code-balloon+
   {9 39 ,(scope-balloon (code (#,(encolors (code lambda) scope3) (#,(encolors (code x) scope3)) x)) #:spike-x* 0.75)}
   #:code-balloon+
   {3 30 ,(scope-balloon (encolors (tt " ") scope3) #:code? #f #:spike 'w)}
   #:code-balloon+
   {5 16 ,(scope-balloon (code (#,(encolors (code thunk) scope3) #,(encolors (code x) scope3))) #:spike 's)}
   #:code-balloon/-
   {7 16 ,(scope-balloon (code (lambda (x) #,(encolors (code x) scope3))) #:spike 's)}
   
   #:example-def
   #:example
   code:blank #:!
   
   ;; ============================================================
   #:part "Part 3 - Core Forms"
   
   ;; ----------------------------------------
   #:section "Primitive Syntactic Forms"

   #:code
   (code:comment "Expand a function call")
   (define (expand-app s env)
     (map (lambda (sub-s) (expand sub-s env))
          s))
   
   #:code-balloon
   {1 21 ,(scope-balloon (code (f (one))))}
   #:code-balloon+
   {3 9 ,(scope-balloon (code (f '1)) #:spike 'nw)}
   
   #:code
   (code:comment "Expand a `lambda` form")
   (define (expand-lambda s env)
     (match-define `(,lambda-id (,arg-id) ,body) s)
     (code:comment "Create a scope for this lambda")
     (define sc (scope))
     (code:comment "Add new scope to the argument identifier")
     (define id (add-scope arg-id sc))
     (code:comment "Bind the argument identifier")
     (define binding (gensym))
     (add-binding! id binding)
     (code:comment "Add binding to the environment")
     (define body-env (env-extend env binding variable))
     (code:comment "Expand the function body after adding the scope")
     (define exp-body (expand (add-scope body sc)
                              body-env))
     (code:comment "Rebuild expanded form")
     `(,lambda-id (,id) ,exp-body))

   #:code-balloon
   {1 24 ,(scope-balloon (code (lambda (x) (f x))) #:square? #t)}
   #:code-balloon+
   {2.5 34 ,(scope-balloon (code x) #:spike 'ne)}
   #:code-balloon+
   {2.5 42 ,(scope-balloon (code (f x)) #:spike 'n)}
   #:code-balloon/--
   {4 12 ,(scope-balloon (encolors (tt " ") scope1) #:spike-y* 1.5 #:code? #f)}
   #:code-balloon/-
   {6 12 ,(scope-balloon x/sc1 #:spike-y* 1.5)}
   #:code-balloon/-
   {8 13 ,(scope-balloon (hbl-append (t "fresh local binding for ")
                                     x/sc1)
                         #:spike-y* 1.5
                         #:code? #f)}
   #:code-balloon/-
   {11 13 ,(scope-balloon (hbl-append (t "fresh binding mapped to ")
                                      (code variable))
                          #:spike-y* 1.5
                          #:code? #f)}
   #:code-balloon/-
   {13 40 ,(scope-balloon (code (f x)) #:spike 's)}
   #:code-balloon/-
   {13 46 ,(scope-balloon (code (#,(encolors (code f) scope1) #,x/sc1)) #:spike 's)}
   
   #:code-balloon+
   {13 14 ,(scope-balloon (code (#,(encolors (code f) scope1) #,x/sc1)) #:spike 's)}
   #:code-balloon+
   {16 30 ,(scope-balloon (code (lambda (#,x/sc1) (#,(encolors (code f) scope1) #,x/sc1)))
                          #:spike 'w
                          #:square? #t)}
   
   #:code
   (code:comment "Expand a local macro-binding form")
   (define (expand-let-syntax s env)
     (match-define `(,let-syntax-id ([,lhs-id ,rhs])
                      ,body)
                   s)
     (code:comment "Create a scope for this let-syntax")
     (define sc (scope))
     (code:comment "Add new scope to the identifier")
     (define id (add-scope lhs-id sc))
     (code:comment "Bind the identifier")
     (define binding (gensym))
     (add-binding! id binding)
     (code:comment "Evaluate compile-time expressions")
     (define rhs-val (eval-for-syntax-binding rhs))
     (code:comment "Map binding to its value")
     (define body-env (env-extend env binding rhs-val))
     (code:comment "Expand body")
     (expand (add-scope body sc) body-env))
   #:code-balloon
   {0.7 27.5 ,(scope-balloon (code (let-syntax ([one (lambda (stx)
                                                       (quote-syntax '1))])
                                     (one)))
                             #:spike 's
                             #:spike-y* 0.8
                             #:square? #t)}
   #:code-balloon+
   {2 46 ,(scope-balloon (code (lambda (stx)
                                 (quote-syntax '1)))
                         #:spike 'n
                         #:square? #t)}
   #:code-balloon+
   {12 13 ,(scope-balloon (t "a procedure")
                          #:spike-y* 1.5
                          #:code? #f)}
   #:code-balloon+
   {6 12 ,(scope-balloon (encolors (tt " ") scope2) #:spike-y* 1.5 #:code? #f)}
   #:code-balloon/+-
   {15 13 ,(scope-balloon (hbl-append (t "fresh binding for ")
                                      (encolors (code one) scope2)
                                      (t " mapped to procedure"))
                          #:spike-y* 1.5
                          #:code? #f)}
   #:code-balloon+
   {17 23 ,(scope-balloon (code (one)) #:spike 's)}
   #:code-balloon/-
   {17 29 ,(scope-balloon (code (#,(encolors (code one) scope2))) #:spike 's)}
   #:code-balloon+
   {17 39 ,(scope-balloon (code (#,(encolors (code quote) scope3) 1)) #:spike 'w)}
   
   #:code
   (code:comment "Helper: expand and eval for compile time")
   (define (eval-for-syntax-binding rhs)
     (eval-compiled (compile (expand rhs empty-env))))
   #:code-balloon
   {1 34 ,(scope-balloon (code (lambda (stx)
                                 (quote-syntax '1)))
                         #:spike 's
                         #:square? #t)}

   ;; ----------------------------------------
   #:section "Bridge to the Host"

   #:code
   (code:comment "Compile expanded to a host-Racket S-expression")
   (define (compile s)
     (cond
      [(identifier? s) (resolve s)]
      [else
       (case (and (identifier? (first s)) (resolve (first s)))
         [(lambda)
          (match-define `(,lambda-id (,id) ,body) s)
          `(lambda (,(resolve id)) ,(compile body))]
         [(quote)
          (match-define `(,quote-id ,datum) s)
          `(QQUOTE ,(syntax->datum datum))]
         [(quote-syntax)
          (match-define `(,quote-syntax-id ,datum) s)
          `(QQUOTE ,datum)]
         [else
          (code:comment "Anything else is a function call")
          (map compile s)])]))
   
   #:code-balloon
   {8 30 ,(scope-balloon (hbl-append (t "use ") (tt "gensym") (t " for variable"))
                         #:code? #f)}
   #:code-balloon
   {11 36 ,(scope-balloon (vc-append (current-line-sep) (t "strip scopes for") (code quote))
                          #:code? #f)}
   #:code-balloon
   {14 21 ,(scope-balloon (vc-append (current-line-sep) (t "preserve scopes for") (code quote-syntax))
                          #:code? #f)}
   #:next
   
   #:code
   (code:comment "Set up a host-Racket evaluation environment")
   (define namespace (make-base-namespace))
   (eval '(require racket/list) namespace)
   code:blank
   (namespace-set-variable-value! 'datum->syntax
                                  datum->syntax
                                  #t namespace)
   (namespace-set-variable-value! 'syntax->datum
                                  syntax->datum
                                  #t namespace)
   (namespace-set-variable-value! 'syntax-e
                                  syntax-e
                                  #t namespace)
   code:blank
   (define (eval-compiled s)
     (eval s namespace))
   
   ;; #:code-balloon
   ;; {1 15 ,(scope-balloon (t "Racket-level namespace") #:code? #f)}
   #:code-balloon+
   {14 24 ,(scope-balloon (code (lambda (g42) g42)))}
   ;; #:code-balloon+
   ;; {15 5 ,(scope-balloon (hbl-append (t "Racket-level ") (code eval)) #:spike 's #:code? #f #:spike-y* 0.5)}

   ;; ============================================================
   #:external/code
   ,(lambda (get-code)
      (define (copy s) (send the-clipboard set-clipboard-string s 0))
      (define (ct s)
        (define p (t s))
        (scale (colorize (vc-append p (linewidth 2 (hline (pict-width p) 0))) "blue") 0.8))
      (slide
       #:name "Done!"
       (titlet "Done!")
       (blank (* 3 gap-size))
       (hc-append (* 4 gap-size)
                  (clickback (ct "Copy Code")
                             (lambda () (copy (get-code #f))))
                  (clickback (ct "Copy Code + Examples")
                             (lambda () (copy (get-code #t)))))))
   
   ))

;; --------------------------------------------------

(module+ main
  (pico-slides))
