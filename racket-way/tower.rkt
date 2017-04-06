#lang slideshow
(require slideshow/code
         slideshow/balloon
         (except-in "../tower/utils.rkt" as-file)
         "../tower/lisp.rkt"
         "../tower/scheme.rkt"
         "../tower/plt.rkt"
         "slope.rkt"
         "as-file.rkt"
         "skip.rkt"
         "colors.rkt")

(provide tower-slides)

(define do-animate? #t)
(define animate-steps 5)

(define-syntax-rule (begin-or-skip/a e ...)
  (if condense?
      (begin (skip-slides (begin 'e (if do-animate? (add1 animate-steps) 1))) ...)
      (begin e ...)))

(define dots (let ([dot (t ".")])
               (hc-append (colorize dot (list 0 0 0))
                          (colorize dot (list 50 50 50))
                          (colorize dot (list 100 100 100))
                          (colorize dot (list 150 150 150))
                          (colorize dot (list 200 200 200)))))
(define-syntax ....
  (make-code-transformer #'dots))

(define (tower-slides
         #:details? [details? #f]
         #:slope? [slope? details?]
         #:pattern-macros? [pattern-macros? details?]
         #:lexical-scope? [lexical-scope? details?]
         #:lexical-scope-how? [lexical-scope-how? #f]
         #:implicit-forms? [implicit-forms? details?]
         #:compile-time? [compile-time? details?]
         #:reader-lang? [reader-lang? details?]
         #:environment-support? [environment-support? details?])

  (slide
   #:title "A Text Adventure Game"
   (apply
    vl-append
    (map
     tt
     '("You're standing in a field."
       "There is a house to the north."
       "> north"
       "You are standing in front of a house."
       "There is a door here."
       "> open door"
       "The door is locked."
       "> "))))

  ;; ----------------------------------------

  (define (et s) (inset (colorize (bt s) "blue") 0 0 (/ gap-size 2) 0))

  (define places-item  (item #:fill? #f (et "Places")))
  (define things-item (item #:fill? #f (et "Things")))
  (define verbs-item (item #:fill? #f (et "Verbs")))

  (slide
   #:title "Implementing a Text Adventure Game"
   'alts
   (let* ([p (vl-append
              gap-size
              places-item
              things-item
              (vl-append
               (* 2 (current-line-sep))
               verbs-item
               (subitem "global intransitive verbs")
               (subitem "place-local intransitive verbs")
               (subitem "thing-specific transitive verbs")))]
          [plain-p p]
          [obj?-label (t "Objects?")]
          [objects (inset obj?-label (* 1/2 gap-size))]
          [meth?-label (t "Methods?")]
          [methods (inset meth?-label (* 1/2 gap-size))]
          [objection (lambda (content)
                       (let ([q (inset
                                 (para #:width (* client-w 0.4)
                                       #:fill? #f
                                       content)
                                 (/ gap-size 2))])
                         (frame
                          (cc-superimpose
                           (colorize (filled-rectangle (pict-width q) (pict-height q))
                                     fail-color)
                           q))))]
          [obj-obj (objection "Need not only serialize, but save & restore variables")]
          [meth-obj (objection "Must convert between string command and method call")]
          [b1 (wrap-balloon objects 'w (* gap-size -3) 0)]
          [b2 (let-values ([(x1 y1) (rc-find p places-item)]
                           [(x2 y2) (rc-find p things-item)])
                (wrap-balloon objects 'w 
                              (+ (* gap-size -3) (- x2 x1))
                              (- y2 y1)))]
          [p (pin-balloon b2 p things-item rc-find)]
          [p (pin-balloon b1 p places-item rc-find)]
          [obj?-p p]
          [b3 (wrap-balloon methods 'nw (* gap-size -3) (* gap-size -3))]
          [p (pin-balloon b3 p verbs-item rc-find)]
          [meth?-p p]
          [p (pin-over p obj?-label rb-find obj-obj)]
          [obj-obj-p p]
          [p (pin-over p meth?-label rb-find meth-obj)]
          [meth-obj-p p])
     (list (list plain-p)
           (list 'alts~
                 (map list (list obj?-p
                                 meth?-p
                                 obj-obj-p
                                 meth-obj-p))))))

  ;; ----------------------------------------

  (when slope?
    (slide
     #:title "A Text Adventure Language"
     (scale
      (code
       #,(tt "===VERBS===")
       #,(hbl-append (code north) (tt ", ") (code n))
       "go north"
       ....
       #,(tt "===EVERYWHERE===")
       quit 
         ....
       #,(tt "===THINGS===")
       ---cactus---
       get
       "Ouch!"
       ....
       #,(tt "===PLACES===")
       ---desert---
       "You're in a desert. There is nothing for miles around."
       [#,(hbl-append (code cactus) (tt ", ") (code key))]
       north   start
       south   desert
       ....)
      0.8))
    (slope-slides))

  ;; ----------------------------------------

  (slide
   #:title "Adventure Game Data"
   (cb-superimpose
    (cc-superimpose
     titleless-page
     (scale
      (code (code:comment "A place is")
            (code:comment " (place symbol list-of-thing dict-of-verb-to-function)")
            (struct place (desc [things #:mutable] actions))
            code:blank
            (code:comment "A thing is")
            (code:comment " (thing symbol any dict-of-verb-to-function)")
            (struct thing (name [state #:mutable] actions))
            code:blank
            (code:comment "A verb is")
            (code:comment " (verb list-of-symbol string boolean)")
            (struct verb (aliases desc transitive?))
            code:blank
            code:blank)
      0.8))
    (parameterize ([current-font-size (floor (* #e0.8 (current-font-size)))])
      (para #:align 'right 
            #:width client-w
            "all code at ``Creating Languages in Racket,''" (it "ACM Queue") ", November 2011"))))

  (define v0-title "Version 0: Longhand")

  (slide
   #:title v0-title
   (scale
    (code
     (define north (verb (list 'north 'n) "go north" #f))
     (record-element! 'north north)
     code:blank
     (define south (verb (list 'south 's) "go south" #f))
     (record-element! 'south south)
     code:blank
     ....
     code:blank
     (define get (verb (list 'get 'grab 'take) "take" #t))
     (record-element! 'get get)
     code:blank
     (define put (verb (list 'put 'drop 'leave) "drop" #t))
     (record-element! 'put put)
     code:blank
     ....)
    0.75))

  (slide
   #:title v0-title
   #:name (string-append v0-title " (2)")
   (scale
    (code
     (define door
       (thing 'door
              #f
              (list
               (cons open 
                     (lambda ()
                       (if (have-thing? key)
                           (begin
                             (set-thing-state! door 'open)
                             "The door is now unlocked and open.")
                           "The door is locked.")))
               (cons close 
                     (lambda ()
                       (begin
                         (set-thing-state! door #f)
                         "The door is now closed.")))
               (cons knock 
                     (lambda ()
                       "No one is home.")))))
     (record-element! 'door door))
    0.75))

  (when pattern-macros?
    (simple-pattern-slides)
    (pattern-slides)
    (pattern-...-slides))

  (define use-sep
    (vc-append
     (tt " ")
     (inset (hline (* client-w 2/3) 0) (* client-w 1/6) 0)
     (tt " ")))

  (define v1-title "Version 1: Syntactic Abstraction")

  (define (3/4t-find p p2)
    (define-values (x y) (ct-find p p2))
    (values (+ x (/ (pict-width p) 4)) y))

  (define animation-progress (make-parameter 1))

  (define (hilite color 
                  #:animate? [animate? #t]
                  #:delta [delta 0]
                  #:spike [spike 'sw]
                  #:spike-dy [spike-dy #f]
                  #:scale [b-scale 1]
                  #:pin-find [pin-find (case spike
                                         [(n) cb-find]
                                         [(s) ct-find]
                                         [(nw) cb-find]
                                         [(sw) ct-find]
                                         [(se) ct-find]
                                         [(w) rc-find])]
                  . content)
    (lambda (p)
      (define new-p
        (if color
            (refocus (cc-superimpose (cellophane
                                      (colorize (filled-rounded-rectangle (+ (+ 6 (pict-width p)) delta)
                                                                          (+ (- (pict-height p) 2) delta)
                                                                          4)
                                                color)
                                      (if animate? (animation-progress) 1))
                                     p)
                     p)
            p))
      (if (or (null? content)
              (and animate? (not (= 1 (animation-progress)))))
          new-p
          (refocus (pin-balloon (wrap-balloon (scale (apply para #:fill? #f content) b-scale)
                                              spike 
                                              (case spike
                                                [(sw nw se n s) 0]
                                                [(w) (- gap-size)])
                                              (or spike-dy
                                                  (case spike
                                                    [(nw n) (- gap-size)]
                                                    [(sw se s) gap-size]
                                                    [(w) 0]))
                                              balloon-color
                                              16)
                                new-p
                                new-p pin-find)
                   new-p))))
    
  (define (annote s p)
    (define lbl (rotate (colorize (bt s) "white") (/ pi 2)))
    (refocus (hc-append
              (/ gap-size 2)
              (cc-superimpose (colorize
                               (filled-rectangle (+ (pict-width lbl) 10)
                                                 (pict-height p))
                               panel-color)
                              lbl)
              p)
             p))

  (define-syntax-rule (animate-slide #:animate? animate? e ...)
    (animate animate?
             (lambda (timeout)
               (slide #:timeout timeout e ...))))

  (define (animate animate? thunk)
    (when animate?
      (for ([i (in-range animate-steps)])
        (parameterize ([animation-progress (/ i 1.0 animate-steps)])
          (when do-animate?
            (if (not condense?)
                (thunk 0.03)
                (skip-slides 1))))))
    (thunk #f))

  (define (dsr-slide #:animate? [animate? #t]
                     #:dsr [dsr values]
                     #:dt [dt values]
                     #:dr [dr values]
                     #:drs [drs values]
                     #:pat [pat values]
                     #:tmpl [tmpl values]
                     #:vrb [vrb values]
                     #:vrbs [vrbs values]
                     #:expr [expr values]
                     #:exprs [exprs values])
    (let-syntax ([define-syntax-rule (make-code-transformer #'(dsr (code define-syntax-rule)))]
                 [define-thing (make-code-transformer #'(dt (code define-thing)))]
                 [_ID (make-code-transformer #'(dr (code _id)))]
                 [_id (make-code-transformer #'(drs (code _id)))]
                 [DOOR (make-code-transformer #'(dr (code door)))]
                 [_VRB (make-code-transformer #'(vrb (code _vrb)))]
                 [_EXPR (make-code-transformer #'(expr (code _expr)))]
                 [...vrb (make-code-transformer (quote-syntax (expr (vrb (code ...)))))]
                 [open (make-code-transformer #'(vrb (code close)))]
                 [close (make-code-transformer #'(vrb (code open)))]
                 [openx (make-code-transformer #'(code open))]
                 [knock (make-code-transformer #'(vrb (code knock)))]
                 [_vrb (make-code-transformer #'(vrbs (code _vrb)))]
                 [_expr (make-code-transformer #'(exprs (code _expr)))]
                 [...vrbs (make-code-transformer (quote-syntax (exprs (vrbs (code ...)))))])
      (animate-slide
       #:animate? animate?
       #:title v1-title
       (scale
        (code
         #,(annote 
            "definition"
            (code
             (define-syntax-rule #,(pat (code (define-thing _ID
                                                [_VRB _EXPR] ...vrb)))
               #,(tmpl
                  (code
                   (begin
                     (define _id 
                       (thing '#,(code _id) #f (list (cons _vrb (lambda () _expr)) ...vrbs)))
                     (record-element! '#,(code _id) _id)))))))
         #,use-sep
         #,(annote
            "use"
            (code (define-thing DOOR
                    [open #,(expr
                             (code
                              (if (have-thing? key)
                                  (begin
                                    (set-thing-state! door 'openx)
                                    "The door is now unlocked and open.")
                                  "The door is locked.")))]
                    [close #,(expr
                              (code
                               (begin
                                 (set-thing-state! door #f)
                                 "The door is now closed.")))]
                    [knock #,(expr (code "No one is home."))]))))
        0.75))))
  (begin-or-skip (dsr-slide #:animate? #f))
  (dsr-slide #:dsr (hilite hilite-color "define a pattern-based macro"))
  (dsr-slide #:pat (hilite patcol "pattern"))
  (dsr-slide #:tmpl (hilite tmplcol "template" #:pin-find 3/4t-find))
  (begin-or-skip/a
   (dsr-slide #:dt (hilite patcol))
   (dsr-slide #:dr (hilite patcol)))
  (dsr-slide #:dr (hilite patcol #:animate? #f) #:drs (hilite tmplcol))
  (begin-or-skip/a (dsr-slide #:vrb (hilite patcol)))
  (dsr-slide #:vrb (hilite patcol #:animate? #f) #:vrbs (hilite tmplcol))
  (dsr-slide #:expr (hilite patcol) #:exprs (hilite tmplcol))

  (define (sr-slide #:animate? [animate? #t]
                    #:spec0 [spec0 values]
                    #:spec1 [spec1 values]
                    #:spec [spec spec0]
                    #:specs [specs values]
                    #:alts1 [alts1 spec]
                    #:alts2 [alts2 spec]
                    #:alt$ [alt$ values]
                    #:ds [ds values]
                    #:sr [sr values]
                    #:dov [dov values]
                    #:lits [lits values]
                    #:res [res values])
    (let-syntax ([syntax-rules (make-code-transformer #'(sr (code syntax-rules)))]
                 [define-syntax (make-code-transformer #'(ds (code define-syntax)))])
      (animate-slide
       #:animate? animate?
       #:title v1-title
       #:name (string-append v1-title " (2)")
       (scale
        (code
         #,(annote
            "definition"
            (code
             (define-syntax-rule (define-verbs _all-id
                                   #,(spec0 (code [_id _spec ...])) ...)
               (begin
                 #,(specs (code (define-one-verb _id _spec ...))) ...
                 (record-element! '#,(code _id) _id) ...
                 (define _all-id (list _id ...))))
             code:blank
             (define-syntax #,(dov (code define-one-verb))
               (syntax-rules #,(lits (code (= _)))
                 [#,(spec1 (code (define-one-verb id (= alias ...) desc)))
                  (define id (verb (list 'id 'alias ...) desc #,(res (code #f))))]
                 [#,(spec1 (code (define-one-verb id _ (= alias ...) desc)))
                  (define id (verb (list 'id 'alias ...) desc #,(res (code #t))))]))))
         #,use-sep
         #,(annote
            "use"
            (code
             (define-verbs all-verbs
               #,(alts1 (code [north (= n) "go north"]))
               #,(spec (code [south (= s) "go south"]))
               ....
               #,(alts2 (code [get #,(alt$ (code _)) (= grab take) "take"]))
               #,(spec (code [put _ (= drop leave) "drop"]))
               ....))))
        0.75))))
  (begin-or-skip
   (sr-slide #:animate? #f))
  (sr-slide #:alts1 (hilite hilite-color "no" (code _) ": intransitive" #:spike 'w)
            #:alts2 (hilite hilite-color "has" (code _) ": transitive" #:spike 'w)
            #:alt$ (hilite hilite-color #:delta 16))
  (begin-or-skip/a
   (sr-slide #:spec0 (hilite patcol))
   (sr-slide #:spec0 (hilite patcol #:animate? #f) #:specs (hilite tmplcol)))
  (sr-slide #:spec0 (hilite patcol #:animate? #f) #:specs (hilite tmplcol #:animate? #f) 
            #:dov (hilite hilite-color "helper macro" #:spike 'w))
  (sr-slide #:ds (hilite hilite-color "define a macro")
            #:sr (hilite hilite-color "match multiple patterns" #:spike 'w)
            #:lits ghost)
  (begin-or-skip/a
   (sr-slide #:spec1 (hilite patcol)))
  (sr-slide #:spec1 (hilite patcol #:animate? #f) #:lits (hilite hilite-color "literals"))
  (sr-slide #:spec1 (hilite patcol #:animate? #f) #:res (hilite hilite-color))

  ;; ----------------------------------------

  (define (se-slide #:animate? [animate? #t]
                    #:dt [dt values]
                    #:tmpl [tmpl values]
                    #:ne [ne values])
    (animate-slide
     #:animate? animate?
     #:title "Version 2: Syntactic Extension"
     (hc-append
      (* 4 gap-size)
      (as-file (tt "txtadv.rkt")
               (scale
                (code
                 #,(tt "#lang racket")
                 (provide define-verbs define-thing
                          define-place define-everywhere
                          ....)
                 ....
                 code:blank
                 (define-syntax-rule #,(dt (code (define-thing _id 
                                                   [_vrb _expr] ...)))
                   #,(tmpl
                      (code
                       (begin
                         (define _id 
                           (thing '#,(code _id) #f
                                  (list (cons _vrb (lambda ()
                                                     _expr))
                                        ...)))
                         (#,(ne (code record-element!)) '#,(code _id) _id)))))
                 code:blank
                 ....)
                0.85))
      (as-file (tt "world.rkt")
               (scale
                (code
                 #,(tt "#lang racket")
                 (require "txtadv.rkt")
                 code:blank
                 (define-verbs ....)
                 (define-everywhere ....)
                 #,(dt (code (define-thing ....))) ...
                 (define-place ....) ...)
                0.85)))))
  (begin-or-skip
   (se-slide #:animate? #f))
  (begin-or-skip/a
   (se-slide #:dt (hilite patcol))
   (se-slide #:dt (hilite patcol #:animate? #f) #:tmpl (hilite tmplcol)))
  (se-slide #:dt (hilite patcol #:animate? #f) #:tmpl (hilite tmplcol #:animate? #f)
            #:ne (hilite hilite-color 
                         "not exported, works anyway: lexical scope" 
                         #:spike 'nw #:scale 1.5))

  (when lexical-scope?
    (lexical-slides))
  (when lexical-scope-how?
    (lexical-how-slides))

  ;; ----------------------------------------

  (define (modlang-slide #:animate? [animate? #t]
                         #:lang [lang values]
                         #:s-exp [s-exp values]
                         #:txtadv [txtadv values]
                         #:r [r-proc values]
                         #:r0 [r0 r-proc]
                         #:mb [mb values])
    (define-syntax-rule (r e) (r-proc (code e)))
    (animate-slide
     #:animate? animate?
     #:title "Version 3: Module Language"
     (ht-append
      (* 4 gap-size)
      (as-file (tt "txtadv.rkt")
               (scale
                (code
                 #,(tt "#lang racket")
                 (provide define-verbs define-thing
                          define-place define-everywhere
                          ....
                          code:blank
                          #,(r0 (code cons)) #,(r first) #,(r rest) #,(r lambda) ....
                          code:blank
                          #,(mb (code #%module-begin)))
                 ....
                 code:blank
                 #;
                 (define-syntax module-begin
                   (syntax-rules (define-verbs define-everywhere)
                     [(_ (define-verbs all-verbs cmd ...)
                         (define-everywhere actions act ...)
                         decl ...
                         id)
                      ....])))
                0.85))
      (as-file (tt "world.rkt")
               (scale
                (code
                 code:blank
                 #,(lang (hbl-append (tt "#lang")
                                     (tt " ")
                                     (s-exp (tt "s-exp"))
                                     (tt " ")
                                     (txtadv (tt "\"txtadv.rkt\""))))
                 code:blank
                 (define-verbs ....)
                 (define-everywhere ....)
                 (define-thing ....) ...
                 (define-place ....) ...)
                0.85)))))
  (begin-or-skip
   (modlang-slide #:animate? #f))
  (modlang-slide #:lang (hilite hilite-color "specify language" #:scale 1.5 #:spike 'se))
  (modlang-slide #:s-exp (hilite hilite-color "still using parentheses" #:spike 'se #:scale 1.5))
  (modlang-slide #:txtadv (hilite hilite-color "only" (tt "txtadv.rkt") "exports available" 
                                  #:spike 'se #:scale 1.5))
  (modlang-slide #:r0 (hilite hilite-color "re-export from" (code racket) #:scale 1.5)
                 #:r (hilite hilite-color))
  (modlang-slide #:mb (hilite hilite-color "macro for whole module body" #:spike 'nw #:scale 1.5))

  (when implicit-forms?
    (implicit-slides))

  ;; ----------------------------------------

  (define v4-title "Version 4: Types")

  (define (animate-values p)
    (cellophane p (animation-progress)))

  (define (strike-through p)
    (refocus (cc-superimpose (cellophane (inset (linewidth 3 (hline (+ (pict-width p) 10) 0))
                                                0 6 0 0)
                                         (animation-progress))
                             p)
             p))

  (define (type-error-slide #:animate? [animate? #t]
                            #:desert-animate? [desert-animate? #f]
                            #:desert [desert (hilite fail-color #:animate? desert-animate?)]
                            #:runtime-error [runtime-error ghost]
                            #:type-error [type-error ghost])
    (animate-slide
     #:animate? animate?
     #:title v4-title
     (code
      (define-place room
        "You're in the house."
        [trophy #,(desert (code desert))]
        ([out house-front]))
      code:blank
      #,(vl-append
         (runtime-error (colorize (it "runtime error: message not understood") "red"))
         (type-error (colorize (it "syntax error: `desert' does not have type `thing'") "red"))))))
  (begin-or-skip
   (type-error-slide #:animate? #f #:desert values))
  (begin-or-skip/a
   (type-error-slide #:desert-animate? #t)
   (type-error-slide #:runtime-error animate-values))
  (type-error-slide #:runtime-error strike-through #:type-error animate-values)

  (when compile-time?
    (define-syntax-slides)
    (rep-code-slides)
    (phase-slides)
    (syntax-case-slides))

  (define (type-slide #:animate? [animate? #t]
                      #:bridge [bridge values]
                      #:bridge0 [bridge0 values]
                      #:bfs [bfs bridge0]
                      #:typed [typed values]
                      #:ds0 [ds0 bridge0]
                      #:typed1 [typed1 values]
                      #:id [id values]
                      #:ds [ds bridge0]
                      #:lam [lam values]
                      #:check-type [check-type values]
                      #:check-type0 [check-type0 bridge0]
                      #:slv [slv values]
                      #:ct [ct values]
                      #:rt [rt values]
                      #:key [key ghost])
    (animate-slide
     #:animate? animate?
     #:title v4-title
     #:name (string-append v4-title " (2)")
     (rb-superimpose
      (scale
       (code
        #,(let-syntax ([begin-for-syntax (make-code-transformer #'(bfs (code begin-for-syntax)))]
                       [define-syntax (make-code-transformer #'(ds (code define-syntax)))]
                       [lambda (make-code-transformer #'(lam (code lambda)))])
            (code
             #,(bridge
                (code
                 (begin-for-syntax 
                  #,(ct (code (struct #,(typed (code typed)) (id type) #:property prop:procedure ....))))))
             code:blank
             #,(bridge
                (code
                 (define-syntax #,(check-type (code check-type))
                   #,(ct
                      (code
                       (lambda (stx)
                         (syntax-case stx ()
                           [(check-type id type)
                            (let ([t #,(slv (code (syntax-local-value #'id)))])
                              (unless (and (typed? t)
                                           (equal? (syntax-e #'type) (typed-type t)))
                                (raise-type-error))
                              #'#,(rt (code id)))])))))))))
        #,use-sep
        #,(let-syntax ([define-syntax (make-code-transformer #'(ds0 (code define-syntax)))]
                       [typed (make-code-transformer #'(typed1 (code typed)))]
                       [_ID (make-code-transformer #'(id (code _id)))]
                       [check-type (make-code-transformer #'(check-type0 (code check-type)))])
            (code
             #,(bridge
                (code
                 (define-syntax-rule (define-thing _id ....)
                   (define-syntax _ID #,(ct (code (typed #'#,(rt (code _gen-id)) "thing"))))
                   #,(rt (code (define _gen-id (thing ....))))
                   ....)))
             code:blank
             #,(bridge
                (code
                 (define-syntax-rule (define-place _id _desc (_thng ...) ....)
                   .... #,(rt (code (place _desc (list (check-type _thng #,(ct (code "thing"))) ...) ....)))
                   ....))))))
       0.7)
      (key
       (inset
        (frame
         #:color "red"
         #:line-width 3
         (inset
          (vl-append
           (* 2 (current-line-sep))
           (para #:fill? #f ((hilite comptime-color) (tt " ")) "= compile time")
           (para #:fill? #f ((hilite runtime-color) (tt " ")) "= run time")
           (para #:fill? #f ((hilite patcol) (tt " ")) "= bridge"))
          (/ gap-size 2)))
        0 0 (- gap-size) (* 2.75 (pict-height (tt " "))))))))
  (begin-or-skip (type-slide #:animate? #f))
  (type-slide #:bfs (hilite hilite-color "start compile-time code" #:scale 1.2))
  (type-slide #:typed (hilite patcol "a compile-time record declaration" #:scale 1.2))
  (begin
    (type-slide #:ds0 (hilite hilite-color) #:typed (hilite patcol)
                #:typed1 (hilite patcol)
                #:id (hilite #f "bind" (code _id) "to a compile-time record" #:spike-dy (* 2 gap-size)))
    (type-slide #:check-type0 (hilite patcol "check type of" (code _thng) #:spike 'nw))
    (type-slide #:ds (hilite hilite-color) #:lam (hilite hilite-color)
                #:check-type0 (hilite patcol)
                #:check-type (hilite patcol "bind to a compile-time function (i.e., macro)"))
    (type-slide #:slv (hilite hilite-color "lookup compile-time value")))
  (type-slide #:rt (hilite runtime-color)
              #:ct (hilite comptime-color)
              #:bridge (hilite patcol #:delta 8)
              #:bridge0 (hilite patcol #:delta -2)
              #:key animate-values)

  ;; ----------------------------------------

  (define txtadv-reader-file
    (as-file 
     (tt "txtadv-reader.rkt")
     (scale
      (code
       #,(tt "#lang racket")
       (require syntax/readerr)
       (provide read-syntax)
       code:blank
       (define (read-syntax src in)
         ...
         (datum->syntax
          #f
          `(module world "txtadv.rkt"
             ....))))
      0.85)))

  (define (lang-slide #:animate? [animate? #t]
                      #:reader [reader values]
                      #:txtadv [txtadv values]
                      #:txtadv-reader [txtadv-reader ghost])
    (define world-file
      (as-file (tt "world.rkt")
               (scale
                (vl-append
                 (hbl-append (tt "#lang")
                             (tt " ")
                             (reader (tt "reader"))
                             (tt " ")
                             (txtadv (tt "\"txtadv-reader.rkt\"")))
                 (tt*
                  "                                          "
                  "===VERBS==="
                  "north, n"
                  "\"go north\""
                  " "
                  "south, s"
                  "\"go south\""
                  " "
                  "...."
                  " "
                  "===THINGS==="
                  " "
                  "---cactus---"
                  "get"
                  " \"Ouch!\""
                  "...."))
                0.85)))
    (animate-slide
     #:animate? animate?
     #:title "Version 5: New Language"
     (refocus
      (rc-superimpose
       world-file
       (inset (txtadv-reader txtadv-reader-file) 
              (- (* 4 gap-size))
              0))
      world-file)))
  (begin-or-skip (lang-slide #:animate? #f))
  (lang-slide #:reader (hilite hilite-color "import character-level parser..." #:scale 1.2))
  (lang-slide #:txtadv (hilite hilite-color "parses into a module that imports" (tt "txtadv.rkt") 
                               #:spike 's #:scale 1.2))
  (lang-slide #:txtadv-reader values  #:animate? #f)

  (when reader-lang?
    (parsing-slides))

  ;; ----------------------------------------

  (define green "forestgreen")
  (define dark-blue "darkblue")

  (define (env-slide #:animate? [animate? #t]
                     #:txtadv [txtadv values])
    (animate-slide
     #:animate? animate?
     #:title "Version 6: Environment Support"
     (as-file (tt "world.rkt")
              (scale
               (vl-append
                (current-line-sep)
                (hbl-append (tt "#lang ")
                            (txtadv (tt "txtadv")))
                (tt*
                 "                    "
                 "===VERBS===")
                (colorize
                 (tt "north, n")
                 dark-blue)
                (colorize
                 (tt "\"go north\"")
                 green)
                (tt " ")
                (colorize
                 (tt "south, s")
                 dark-blue)
                (colorize
                 (tt "\"go south\"")
                 green)
                (tt*
                 " "
                 "...."
                 " "
                 "===THINGS==="
                 " ")
                (colorize
                 (tt* "---cactus---"
                      "get")
                 dark-blue)
                (colorize
                 (tt " \"Ouch!\"")
                 green)
                (tt "...."))
               0.85))))
  (begin-or-skip (env-slide #:animate? #f))
  (env-slide #:txtadv (hilite hilite-color "installed with" (tt "raco link") #:scale 1.2))

  (when environment-support?
    (slide
     #:title "Environment Support"
     (para "Support at S-expression level is free")
     (item "Error source locations")
     (item "Check Syntax")
     (blank)
     (para "Source-editing support requires more")
     (item "On-the-fly coloring"))))

;; ----------------------------------------

(module+ main
  (tower-slides #:details? #t))
