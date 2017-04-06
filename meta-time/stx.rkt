#lang slideshow
(require slideshow/code
         "style.rkt"
         "in-file.rkt")

(provide stx-slides
         stx-phase-slides)

(define result-color "darkblue")

(define (format-error str #:width [width (* 3/4 client-w)])
  (colorize (parameterize ([current-main-font 
                            `(italic . ,(current-main-font))])
              (para #:width width
                    #:fill? #f
                    (regexp-replace* #rx"[\r\n] *" str " ")))
            "red"))

(define-syntax-rule (repl [arg ...] [d ...] e ...)
  (repl* arg ...
         (list (code d) ...)
         (list 'd ...)
         (list (list (code e) 'e)
               ...)))

(define (repl* #:title [title #f] dps ds l)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require '(for-syntax racket/base)))
  (for ([d (in-list ds)])
    (eval (clean-for-eval d) ns))
  (define-values (a seq)
    (for/fold ([a (apply vl-append
                         (current-code-line-sep)
                         (for/list ([dp (in-list dps)])
                           (htl-append (tt "> ") dp)))]
               [l null])
        ([p (in-list l)])
      (define new-a
        (vl-append
         (current-code-line-sep)
         a
         (blank (/ gap-size 2))
         (htl-append (tt "> ") (car p))
         (with-handlers ([exn:fail?
                          (lambda (exn)
                            (format-error #:width (* 3/4 client-w) 
                                          (exn-message exn)))])
           (define v (eval (clean-for-eval (cadr p)) ns))
           (if (void? v)
               (blank)
               (parameterize ([error-print-source-location #f])
                 (colorize (tt (format "~v" v)) result-color))))))
      (values new-a
              (cons new-a l))))
  (slide
   #:title title
   'alts
   (align (reverse seq))))

(define (align l)
  (define w (max (current-para-width)
                 (for/fold ([w 0]) ([p (in-list l)])
                   (max w (pict-width p)))))
  (for/list ([p (in-list l)])
    (list (inset p 0 0 (- w (pict-width p)) 0))))

(define-syntax-rule (mods [arg ...] (name body ...) ...)
  (mods* arg ...
         (list name ...)
         '((body ...) ...)
         (list (code body ...) ...)))

(define-syntax-rule (mod-repl (arg ...) [def ...] form ...)
  (mod-repl* arg ...
             '(def ...)
             (code def ...)
             '(form ...)
             (list (code form) ...)))

(define (mod-repl* #:lang [lang #f] 
                   #:title [title #f]
                   defs def-codes forms form-codes)
  (for ([a-form (in-list forms)]
        [a-form-code (in-list form-codes)])
    (mods* #:lang lang
           #:title title
           (list "main")
           (list (append defs (list a-form)))
           (list
            (vl-append (current-code-line-sep)
                       def-codes
                       a-form-code)))))

(define (clean-for-eval e)
  (cond
   [(list? e)
    (filter (lambda (v)
              (not (and (pair? v)
                        (eq? (car v) 'code:comment))))
            (map clean-for-eval (remq* '(code:blank) e)))]
   [(eq? e 'Quote) 'quote]
   [(eq? e 'Syntax) 'syntax]
   [else e]))

(define (mods* names bodys body-codes
               #:title [title #f]
               #:scale [body-scale 1.0]
               #:lang [lang #f]
               #:skip [skip 0]
               #:filter-result [filter-result values])
  (define ns (make-base-namespace))
  (define o (open-output-string))
  (define e (open-output-string))
  (define (name->path name)
    (path->complete-path (string-append name ".rkt")))
  (call-with-continuation-prompt
   (lambda ()
     (parameterize ([current-output-port o]
                    [current-error-port e]
                    [error-print-source-location #f]
                    [error-print-context-length 0]
                    [uncaught-exception-handler
                     (lambda (exn)
                       (when (exn:break? exn) (exit))
                       ((error-display-handler) (exn-message exn) exn)
                       ((error-escape-handler)))])
       (for ([name (in-list names)]
             [body (in-list bodys)])
         (parameterize ([current-module-declare-name
                         (make-resolved-module-path
                          (name->path name))])
           (eval `(module x ,(or (if lang 
                                     (case lang
                                       ['racket/base "base.rkt"]
                                       ['racket "base2.rkt"]
                                       [else lang])
                                     "base2.rkt"))
                    . ,(clean-for-eval body)) ns)))
       (parameterize ([current-namespace ns])
         (dynamic-require (name->path (last names)) #f)))))
  (slide
   #:title title
   #:layout 'top
   (vc-append
    gap-size
    (apply ht-append
           gap-size
           (values ; reverse
            (for/list ([name (in-list (list-tail names skip))]
                       [body-code (in-list (list-tail body-codes skip))])
              (mk-file #:name name #:suffix "rkt"
                       (scale (if lang
                                  (vl-append
                                   (current-code-line-sep)
                                   (hbl-append (tt "#lang ") 
                                               (colorize (tt (format "~a" lang))
                                                         (current-id-color)))
                                   body-code)
                                  body-code)
                              body-scale)))))
    (if (string=? "" (get-output-string o))
        (blank)
        (blank gap-size))
    (para
     (apply vl-append
            (current-code-line-sep)
            (for/list ([s (regexp-split #rx"[\r\n]" (filter-result
                                                     (get-output-string o)))])
              (colorize (tt s) result-color))))
    (para
     (format-error (get-output-string e))))))

(define (filter-mpi s)
  (regexp-replace* "[(]#<module-path-index> [^)]*[)]"
                   s
                   "(#<module-path-index> ....)"))

(define (stx-slides)

  (repl
   [#:title "Variables and Symbols"]
   [(define x 42)]
   x
   'x
   (eval 'x)
   (let ([y -1])
     (eval 'y)))

  (repl
   [#:title "Variables, Symbols, and Identifiers"]
   [(define x 42)]
   x
   'x
   #'x)

  (let-syntax ([Quote (make-code-transformer #'(code quote))]
               [Syntax (make-code-transformer #'(code syntax))])
    (repl
     [#:title "Reader Shorthands"]
     [(define x 42)]
     (list (Quote x) 'x)
     (list (Syntax x) #'x)))

  (mods
   [#:title "Modules"]
   ["x"
    (define x 42)    
    x
    'x
    #'x])

  (mods
   [#:title "Binding and Symbols"]
   ["x"
    (define x 42)    
    (eval 'x)])

  (mods
   [#:title "Binding and Identifiers"]
   ["x"
    (define x 42)    
    (eval #'x)])

  (mods
   [#:title "Binding Across Module Boundaries"]
   ["x"
    (define x 42)
    (define x-stx #'x)
    (provide x-stx)]
   ["main"
    (require "x.rkt")
    (eval x-stx)])

  (mod-repl
   [#:title "Expressions, Lists, and Syntax Objects"]
   []
   (f (add1 z))
   '(f (add1 z))
   #'(f (add1 z)))

  (let ([unsyntax 'other])
    (mod-repl
     [#:title "Manipulating Syntax Objects"]
     []
     (syntax-e #'x)
     (syntax-e #'(f (add1 z)))
     (datum->syntax #'x 'f)))

  (mods
   [#:scale 0.8
            #:title "Manipulating Syntax Objects"]
   ["x"
    (define x 42)
    (define y-stx #'y)
    (provide y-stx)]
   ["main"
    (require "x.rkt")
    (eval (datum->syntax y-stx 'x))])

  (mod-repl
   [#:title "Macros"]
   [(define-syntax now
      (lambda (stx)
        #'(current-seconds)))]
   now)

  (mod-repl
   [#:title "Macro Triggers"]
   [(define-syntax (now stx)
      #'(current-seconds))]
   now
   (now)
   (now x y z))

  (mod-repl
   [#:title "Macro Argument"]
   [(define-syntax (now stx)
      (if (symbol? (syntax-e stx))
          #'(current-seconds)
          (raise-syntax-error 'now "bad syntax")))]
   now
   (now))

  (mod-repl
   [#:title "Compile-Time Expressions"]
   [(define-syntax (then stx)
      (current-seconds))]
   then)
  (mod-repl
   [#:title "Compile-Time Expressions"]
   [(define-syntax (then stx)
      (datum->syntax #'here (current-seconds)))]
   then)

  (let ([unsyntax 'other])
    (mod-repl
     [#:title "Run Time vs. Compile Time"]
     [(define-syntax (now stx)
        #'(current-seconds))
      (define-syntax (then stx)
        (datum->syntax #'here (current-seconds)))
      now
      then
      (sleep 1)
      now]
     then))

  (let ([unsyntax 'other])
    (mod-repl
     [#:title "Quasisyntax"]
     [(define-syntax (about-then stx)
        #`(- #,(current-seconds) 10))
      code:blank]
     about-then))

  (let ([unsyntax 'other])
    (mod-repl
     [#:title "Quasisyntax as Coersion"]
     [(define-syntax (then stx)
        #`#,(current-seconds))
      code:blank]
     then))

  (void))

(define (stx-phase-slides)

  (mods
   [#:title "Language as Initial Import"
            #:lang 'racket]
   ["now"
    code:blank
    (define-syntax now
      (lambda (stx)
        #'(current-seconds)))
    code:blank
    now])

  (mods
   [#:lang 'racket/base
           #:title "Empty Compile-time Import"]
   ["now"
    code:blank
    (define-syntax now
      (lambda (stx)
        #'(current-seconds)))
    code:blank
    now])

  (mods
   [#:lang 'racket/base
           #:title "Explicit Compile-time Import"]
   ["now"
    (require (for-syntax racket/base))
    code:blank
    (define-syntax now
      (lambda (stx)
        #'(current-seconds)))
    code:blank
    now])

  (mods
   [#:lang 'racket/base
           #:scale 0.75
           #:title "Expansion to Run-time Import"]
   ["recent"
    code:blank
    (define (recent-seconds)
      (- (current-seconds)
         10))
    code:blank
    (provide recent-seconds)]
   ["main"
    (require (for-syntax racket/base)
             "recent.rkt")
    code:blank
    (define-syntax (recently stx)
      #'(recent-seconds))
    code:blank
    recently])

  (let ([unsyntax 'other])
    (mods
     [#:lang 'racket/base
             #:scale 0.75
             #:title "Misuse of Run-time Import"]
     ["recent"
      code:blank
      (define (recent-seconds)
        (- (current-seconds)
           10))
      code:blank
      (provide recent-seconds)]
     ["main"
      (require (for-syntax racket/base)
               "recent.rkt")
      code:blank
      (define-syntax (about-then stx)
        #`#,(recent-seconds))
      code:blank
      about-then]))

  (let ([unsyntax 'other])
    (mods
     [#:lang 'racket/base
             #:scale 0.75
             #:title "Expansion using Compile-time Import"]
     ["recent"
      code:blank
      (define (recent-seconds)
        (- (current-seconds)
           10))
      code:blank
      (provide recent-seconds)]
     ["main"
      (require (for-syntax racket/base
                           "recent.rkt"))
      code:blank
      (define-syntax (about-then stx)
        #`#,(recent-seconds))
      code:blank
      about-then]))

  (let ([unsyntax 'other])
    (mods
     [#:lang 'racket/base
             #:scale 0.7
             #:title "Variable and Macro Imports are the Same"]
     ["recent"
      (require (for-syntax
                racket/base))
      code:blank
      (define-syntax recent-seconds
        (lambda (stx)
          #'(- (current-seconds)
               10)))
      code:blank
      (provide recent-seconds)]
     ["main"
      (require (for-syntax racket/base
                           "recent.rkt"))
      code:blank
      (define-syntax (about-then stx)
        #`#,(recent-seconds))
      code:blank
      about-then]))

  (let ([unsyntax 'other])
    (mods
     [#:lang 'racket/base
             #:scale 0.75
             #:title "Macro-generating Macros"]
     ["main"
      (require (for-syntax racket/base))
      code:blank
      (define-syntax (define-now-alias stx)
        (define id (cadr (syntax->list stx)))
        #`(define-syntax (#,id stx)
            (if (symbol? (syntax-e stx))
                #'(current-seconds)
                (raise-syntax-error 'id "bad"))))
      code:blank
      (define-now-alias now)
      (define-now-alias at-this-moment)
      code:blank
      at-this-moment]))

  (let ([unsyntax 'other])
    (mods
     [#:lang 'racket/base
             #:scale 0.75
             #:skip 1
             #:title "Macro-generating Macros and Imports"]
     ["recent"
      code:blank
      (define (recent-seconds [delta 10])
        (- (current-seconds)
           delta))
      code:blank
      (provide recent-seconds)]
     ["main"
      (require (for-syntax racket/base)
               "recent.rkt")
      code:blank
      (define-syntax (define-recently-alias stx)
        (define id (cadr (syntax->list stx)))
        #`(define-syntax (#,id stx)
            #'(recent-seconds)))
      code:blank
      (define-recently-alias recently)
      (define-recently-alias lately)
      code:blank
      lately]))

  (let ([unsyntax 'other])
    (mods
     [#:lang 'racket/base
             #:scale 0.75
             #:skip 1
             #:title "Macro-generating Macros and Imports"]
     ["recent"
      code:blank
      (define (recent-seconds)
        (- (current-seconds) 10))
      code:blank
      (provide recent-seconds)]
     ["main"
      (require (for-syntax racket/base
                           "recent.rkt"))
      code:blank
      (define-syntax (define-about-then-alias stx)
        (define id (cadr (syntax->list stx)))
        #`(define-syntax (#,id stx)
            #`#,(recent-seconds)))
      code:blank
      (define-about-then-alias then-ish)
      code:blank
      then-ish]))

  (let ([unsyntax 'other])
    (mods
     [#:lang 'racket/base
             #:scale 0.75
             #:skip 1
             #:title "Macro-generated Uses of Macro-generating Macros"]
     ["recent"
      code:blank
      (define (recent-seconds [delta 10])
        (- (current-seconds)
           delta))
      code:blank
      (provide recent-seconds)]
     ["main"
      (require (for-syntax racket/base
                           "recent.rkt"))
      code:blank
      (define-syntax (defstx stx)
        #`(define-syntax #,@(cdr (syntax-e stx))))
      code:blank
      (defstx (define-about-then-alias stx)
        (define id (cadr (syntax->list stx)))
        #`(defstx (#,id stx)
            #`#,(recent-seconds)))
      code:blank
      (define-about-then-alias then-ish)
      code:blank
      then-ish]))

  (define phase-specific-title
    "Phase-specific Bindings")

  (mods
   [#:skip 1
           #:filter-result filter-mpi
           #:lang 'racket/base
           #:title phase-specific-title]
   ["recent"
    code:blank
    (define (recent-seconds)
      (- (current-seconds)
         10))
    code:blank
    (provide recent-seconds)]
   ["main"
    (require "recent.rkt")
    code:blank
    (define id #'recent-seconds)
    code:blank
    (identifier-binding id)
    (identifier-transformer-binding id)])

  (mods
   [#:skip 1
           #:filter-result filter-mpi
           #:lang 'racket/base
           #:title phase-specific-title]
   ["recent"
    code:blank
    (define (recent-seconds)
      (- (current-seconds)
         10))
    code:blank
    (provide recent-seconds)]
   ["main"
    (require (for-syntax "recent.rkt"))
    code:blank
    (define id #'recent-seconds)
    code:blank
    (identifier-binding id)
    (identifier-transformer-binding id)])

  (mods
   [#:skip 1
           #:filter-result filter-mpi
           #:lang 'racket/base
           #:title phase-specific-title]
   ["recent"
    code:blank
    (define (recent-seconds)
      (- (current-seconds)
         10))
    code:blank
    (provide recent-seconds)]
   ["main"
    (require "recent.rkt"
             (for-syntax "recent.rkt"))
    code:blank
    (define id #'recent-seconds)
    code:blank
    (identifier-binding id)
    (identifier-transformer-binding id)])

  (let ([unsyntax 'other])
    (mods
     [#:lang 'racket/base
             #:title "Different Phases, Different Bindings"]
     ["main"
      (require (for-syntax
                (rename-in racket/base
                           [caddr cadr]
                           [cadr caddr])))
      code:blank
      (define-syntax (choose stx)
        #`(cadr #,(cadr (syntax-e stx))))
      code:blank
      (choose '(choice 1 2) '(choice a b))]))

  (mods
   [#:lang 'racket
           #:scale 0.8
           #:skip 1
           #:title "Phases and State"]
   ["now"
    (provide now)
    (define-syntax (now stx)
      #'(current-seconds))]
   ["time"
    (require "now.rkt")
    (provide done!)
    code:blank
    (define start-time now)
    code:blank
    (define (done!)
      (displayln
       (- now start-time)))]
   ["main"
    (require "time.rkt"
             (for-syntax "time.rkt"))
    code:blank
    (define-syntax (slow stx)
      (code:comment "simulate slow compile:")
      (sleep 3)
      (done!)
      (code:comment "simulate slow run:")
      #'(sleep 2))
    (slow)
    code:blank
    (done!)])

  (void))

(module+ main
  (stx-slides)
  (stx-phase-slides))
