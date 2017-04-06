#lang at-exp slideshow
(require slideshow/code
         "style.rkt"
         "in-file.rkt"
         racket/gui/base
         framework
         "rep.rkt"
         syntax/modread
         racket/date)

(provide design-slides
         short-design-slides)

(define (scale-font-size s)
  (inexact->exact (floor (* (car (current-expected-text-scale)) s))))

(define default-font-size (scale-font-size 24))
(define smaller-font-size (scale-font-size 22))

(define result-editor 
  (new (class repl-text%
         (define/override (get-prompt) "")
         (super-new))))
(define result-custodian (make-custodian))

(define available (make-hash))
(define (register-available! name t)
  (hash-set! available name t))
(define (unregister-available! name)
  (hash-remove! available name))
(define current-loading-modules (make-parameter null))

(define (do-eval name t sm)
  (custodian-shutdown-all result-custodian)
  (set! result-custodian (make-custodian))
  (send result-editor reset-console)
  (for ([e (in-hash-values available)])
    (send e unhighlight-ranges void))

  (define ns (make-base-namespace))
  (namespace-attach-module (current-namespace) 'errortrace/errortrace-key ns)
  
  (yield
   (parameterize ([current-custodian result-custodian]
                  [current-namespace ns]
                  [current-module-name-resolver
                   (let ([orig (current-module-name-resolver)])
                     (case-lambda
                      [(a b) (orig a b)]
                      [(path rel-to stx load?)
                       (define t (and (string? path)
                                      (hash-ref available (path->complete-path path) #f)))
                       (define rp (and t (make-resolved-module-path
                                          (path->complete-path path))))
                       (if (and rp (not (module-declared? rp)))
                           (let ([loading (current-loading-modules)]
                                 [name (resolved-module-path-name rp)])
                             (when (member name loading)
                               (error "cycle in loading modules"))
                             (parameterize ([current-loading-modules
                                             (cons name (current-loading-modules))])
                               (do-one-eval name t))
                             rp)
                           (orig path rel-to stx load?))]))]
                  [current-output-port
                   (send result-editor get-value-port)]
                  [current-error-port
                   (send result-editor get-err-port)]
                  [exit-handler (lambda (v)
                                  (custodian-shutdown-all result-custodian))])
     (thread
      (lambda ()
        (dynamic-require 'errortrace #f)
        (with-handlers ([exn:fail? (lambda (x)
                                     (send result-editor
                                           show-error
                                           x))])
          (do-one-eval name t)
          (dynamic-require (if sm `(submod ,name ,sm) name) 0)
          (flush-output (send result-editor get-value-port))))))))

(define (do-one-eval name t)
  (define txt (send t get-text))
  (call-with-output-file "eval-log.rktl"
    #:exists 'append
    (lambda (o)
      (fprintf o ";; @ ~a -----------\n"
               (date->string (seconds->date (current-seconds))
                             #t))
      (fprintf o ";; ~a\n" name)
      (display txt o)
      (newline o)))
  (define p (open-input-text-editor t #:lock-while-reading? #t))
  (port-count-lines! p)
  (parameterize ([error-print-source-location #f])
      (define e
        (with-module-reading-parameterization
         (lambda ()
           (parameterize ([read-accept-reader #t])
             (define e (read-syntax t p))
             (when (or (eof-object? e)
                       (not (eof-object? (read p))))
               (error "bad module"))
             (check-module-form e
                                'module
                                "module")))))
      (parameterize ([current-module-declare-name
                      (make-resolved-module-path name)])
        (eval e))))

(define (interactive-module #:name [base-name #f]
                            . init-lines)
  (define t (new (class slide:text%
                   (inherit unhighlight-ranges)
                   (define/private (clear-highlights)
                     (unhighlight-ranges void))
                   (define/augment (on-insert s l)
                     (clear-highlights)
                     (inner (void) on-insert s l))
                   (define/augment (on-delete s e)
                     (clear-highlights)
                     (inner (void) on-delete s e))
                   (super-new))))
  (for ([s (in-list init-lines)]
        [i (in-naturals)])
    (send t insert s)
    (unless (= (add1 i) (length init-lines))
      (send t insert "\n")))
  (define km (send t get-keymap))
  (define ((create font-size auto-eval?) win)
    (send result-editor reset-console)
    (set-font! font-size
               ;; face/family
               (let loop ([l (current-code-font)])
                 (if (pair? l) (loop (cdr l)) l))
               ;; bold?
               (let loop ([l (current-code-font)])
                 (and (pair? l) 
                      (or (eq? 'bold (car l)) 
                          (loop (cdr l))))))
    (define name (path->complete-path
                  (format "~a.rkt" base-name)))
    (send km add-function "run"
          (lambda (v e)
            (do-eval name t #f)))
    (send km add-function "run-test"
          (lambda (v e)
            (do-eval name t 'test)))
    (send km map-function "f5" "run")
    (send km map-function "f6" "run-test")
    (define c 
      (new editor-canvas% 
           [parent win]
           [editor t]
           [style '(auto-vscroll auto-hscroll no-border)]))
    (send c set-canvas-background
          (make-object color% file-background))
    (register-available! name t)
    (when auto-eval?
      (queue-callback (lambda ()
                        (do-eval name t #f))
                      #f))
    (lambda ()
      (send c set-editor #f)
      (unregister-available! name)))
  (lambda (w h 
             #:font-size [font-size default-font-size]
             #:auto-eval? [auto-eval? #f])
    (define content (interactive (blank w h)
                                 (create font-size auto-eval?)))
    (mk-file #:name base-name
             #:suffix "rkt"
             content)))

(define (result-area)
  (interactive (blank (* client-w 2/3) (* client-h 1/4))
               (lambda (win)
                 (send result-editor reset-console)
                 (define c 
                   (new editor-canvas% 
                        [parent win]
                        [editor result-editor]
                        [style '(auto-vscroll auto-hscroll no-border)]))
                 (lambda () (send c set-editor #f)))))

(define now-mod
  (interactive-module
   #:name "now"
   "#lang racket"
   ""
   "(define now"
   "  (current-seconds))"
   ""
   "now"))

(define timecard-mod
  (interactive-module
   #:name "today"
   "#lang racket"))

(define then-mod
  (interactive-module
   #:name "then"
   "#lang racket"
   ""
   "(define-syntax (then stx)"
   "  (current-seconds))"
   ""
   "then"))

(define recent-mod
  (interactive-module
   #:name "recent"
   "#lang racket/base"
   ""
   "(define (recent-seconds)"
   "  (- (current-seconds) 10))"
   ""
   "(provide recent-seconds)"))

(define about-then-mod
  (interactive-module
   #:name "about-then"
   "#lang racket/base"
   "(require (for-syntax racket/base"
   "                     \"recent.rkt\"))"
   ""
   "(define-syntax (about-then stx)"
   "  #`#,(recent-seconds))"
   ""
   "about-then"))

(define clock-mod
  (interactive-module
   #:name "clock"
   "#lang racket"
   ""
   "\"tick\""))

(define tick-mod
  (interactive-module
   #:name "tick"
   "#lang racket"
   ""
   "(define sound \"tick\")"
   ""
   "(define id 'sound)"
   "(provide id)"))

(define tock-mod
  (interactive-module
   #:name "tock"
   "#lang racket"
   ""
   "(define sound \"tock\")"
   ""
   "(require \"tick.rkt\")"))

(define now-alias-mod
  (interactive-module
   #:name "main"
   @string-append{
                    #lang racket
                          
                    (define-syntax (define-now-alias stx)
                      (define id (cadr (syntax-e stx)))
                      #`(define-syntax (#,id stx)
                          #'(current-seconds)))
                      
                    (define-now-alias now)
                    (define-now-alias at-this-moment)
                          
                    at-this-moment
                   }))

(define times-mod
  (interactive-module
   #:name "times"
   @string-append{
                  #lang racket

                  (define-syntax (define-times stx)
                    (define rt-id (cadr (syntax-e stx)))
                    (define ct-id (caddr (syntax-e stx)))
                    (define proc (cadddr (syntax-e stx)))
                    #`(begin
                        (define-syntax (#,rt-id stx)
                          #'(#,proc (current-seconds)))
                        (define-syntax (#,ct-id stx)
                          #`#,(#,proc (current-seconds)))
                        (provide #,rt-id #,ct-id)
                        (module+ test
                          (require rackunit)
                          (check-equal? #,rt-id (begin (sleep 1) (- #,rt-id 1)))
                          (check-equal? #,ct-id (begin (sleep 1) #,ct-id)))))

                  (provide define-times)
                 }))

(define both-mod
  (interactive-module
   #:name "main"
   @string-append{
                  #lang racket
                  (require "times.rkt")
                  (define-times recently about-then
                    (lambda (t) (- t 10)))
                  recently
                 }))

(define binding-mod
  (interactive-module
   #:name "main"
   @string-append{
                    #lang racket
                    (require "recent.rkt")

                    (define id #'recent-seconds)
                          
                    (identifier-binding id)
                    (identifier-transformer-binding id)
                 }))

(define past-mod
  (interactive-module
   #:name "past"
   "#lang racket"
   ""
   "(define now"
   "  (current-seconds))"
   " "
   "(define-syntax (past stx)"
   "  #'(- (current-seconds)"
   "       now))"
   ""
   "(provide past)"))

(define pause-mod
  (interactive-module
   #:name "pause"
   "#lang racket"
   "(require \"past.rkt\")"
   ""
   "past"
   "(sleep 1)"
   "past"))

(define small-w (* client-w 1/3))
(define small-h (* client-h 1/3))

(define med-w (* client-w 1/2))
(define med-h (* client-h 1/2))

(define big-w (* client-w 2/3))
(define big-h (* client-h 1/2))

(define (two-modules a-mod b-mod
                     #:width [width small-w]
                     #:left-width [left-width width]
                     #:right-width [right-width width]
                     #:height [height small-h])
  (hc-append
   gap-size
   (a-mod left-width height)
   (b-mod right-width height)))

(define (design-slides)
  (define (now-mod-slide auto-eval?)
    (slide
     #:title "Module"
     (now-mod small-w small-h #:auto-eval? auto-eval?)
     (result-area)))
  (now-mod-slide #f)
  (now-mod-slide #t)
  ;; In the language `racket'
  ;; Definition
  ;; Expression ... result is printed
  ;;  now (sleep 1) now

  (slide
   #:title "Modules"
   (two-modules now-mod timecard-mod)
   (result-area))
  ;; `require', `provide', no cycles

  (slide
   #:title (hbl-append (titlet "The ") (code module) (titlet " Form"))
   #:name "The module Form"
   (clock-mod big-w big-h)
   (result-area))
  ;; #lang -> `module'
  ;; nested modules `tick' and `tock'
  ;; declaration vs instantiation
  ;; (module* test #f ...) [and hit F6]
  #;
  (module clock.rkt racket
    (module tick racket
      'tick)
    (module tock racket
      (define sound 'tock)
      (module* sub #f
        (require (submod ".."))
        sound))
    (require (submod "." tock sub)))

  (slide
   #:title "Symbols and Syntax Objects"
   (two-modules tick-mod tock-mod)
   (result-area))
  ;; Symbols have no scope...
  ;;  (eq? id sound)
  ;;  (eval 'sound)
  ;;  (eval #'sound)
  ;;  (eval id)
  ;; Note that ' is an abbreviation for `quote' [in `tick']
  ;; Show that #' is an abbreviation for `quote-syntax' [in `tick']
  ;;  #`(list #,id "!")
  ;;  (eval #`(list #,id "!")) [in `tock']

  (slide
   #:title "Macros"
   (now-mod med-w med-h)
   (result-area))
  ;; Try `now (sleep 1) now'
  ;; Change `now' to a macro
  ;;  Aside:
  ;;   (if (symbol? (syntax-e stx))
  ;;       #'(current-seconds)
  ;;       (raise-syntax-error 'now "bad syntax"))
  ;;  Change it back for simplicity


  (slide
   #:title "Compile-Time Expressions"
   (then-mod med-w med-h)
   (result-area))
  ;; Fix bug by using #`#,
  ;; `then', `(sleep 1)' , `then'
  ;; Define `around-then' using quasisyntax

  (slide
   #:title "Compile-Time Imports"
   (then-mod big-w med-h)
   (result-area))
  ;; Change `racket' to `racket/base'
  ;;  ... add `(require (for-syntax racket/base))'

  (slide
   #:title "Compile-Time Imports"
   (vc-append gap-size
              (recent-mod #:font-size smaller-font-size big-w (* client-h 1/4))
              (about-then-mod #:font-size smaller-font-size big-w small-h))
   (result-area))
  ;; Mangle import of "recent.rkt"
  ;; Change `recently' into a macro

  (slide
   #:title "Macro-Generating Macros"
   (now-alias-mod big-w big-h)
   (result-area))

  (slide
   #:title "Macro-Generating Macros and Imports"
   (vc-append
    (/ gap-size 2)
    (recent-mod #:font-size smaller-font-size big-w (* client-h 1/6))
    (now-alias-mod #:font-size smaller-font-size big-w (- big-h (* 2 gap-size))))
   (result-area))
  ;; Simplify "recent.rkt" back
  ;; Convert alias creator to `recently'
  ;; Convert alias creator to `about-then'

  (slide
   #:title "Everything-Generating Macros"
   (inset (times-mod #:font-size smaller-font-size
                     (* 0.9 client-w)
                     (- big-h (* 2 gap-size)))
          0 (- (* 3 gap-size)) 0 0)
   (both-mod #:font-size smaller-font-size big-w (* gap-size 6))
   (result-area))
  ;; Demonstrate test failure?

  (slide
   #:title "Bindings and Phases"
   (vc-append gap-size
              (recent-mod #:font-size smaller-font-size big-w (* gap-size 7))
              (binding-mod #:font-size smaller-font-size big-w (- small-h (* 1 gap-size))))
   (result-area))
  ;; change `require' to `for-syntax'
  ;; both `for-syntax' and plain

  (void))


(define (short-design-slides)
  (define (now-mod-slide auto-eval?)
    (slide
     #:title "Module"
     (now-mod small-w small-h #:auto-eval? auto-eval?)
     (result-area)))
  (now-mod-slide #f)
  (now-mod-slide #t)
  ;; In the language `racket'
  ;; Definition
  ;; Expression ... result is printed
  ;; `(- now)` to demo result

  (slide
   #:title "Modules"
   (two-modules now-mod timecard-mod)
   (result-area))
  ;; `require', `provide'

  (slide
   #:title "Exported Macros"
   (two-modules past-mod pause-mod
                #:left-width (quotient (+ med-w small-w) 2)
                #:height med-h)
   (result-area))
  ;; macro-introduced reference to unexported
  ;; `begin-for-syntax' by moving macro body to `gen'

  (slide
   #:title (hbl-append (titlet "The ") (code module) (titlet " Form"))
   #:name "The module Form"
   (clock-mod big-w big-h)
   (result-area))
  ;; #lang -> `module'
  ;; nested `module test racket' with "tock" [and hit F6]
  ;; declaration vs instantiation
  ;; no access to enclosing module's binding:
  ;;  add `(define tick "tick")` and `tick`
  ;;  and try to use tick in `test`
  ;; (module* test #f ...) [and hit F6]
  ;; use `identifier-binding` and #' instead of direct
  ;; use `begin-for-syntax`: `identifier-binding` now reports #f
  ;;   so switch to `identifier-template-binding`
  #;
  (module clock racket
    (define tick "tick")
    tick
    (module* test #f
      (identifier-template-binding #'tick)))

  (void))

(module+ main
  (short-design-slides))
