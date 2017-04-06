#lang slideshow
(require slideshow/play
         "../meta-time/scripting.rkt"
         "../meta-time/problem.rkt"
         "../meta-time/use-lex-scope.rkt"
         "lang-tree.rkt"
         "utils.rkt"
         "bear.rkt"
         "lesson.rkt")

(provide module+phase-slides)

#|

Intro to defmacro:

 ----------------------------------------

Start with

  #lang racket              ; tell audience to ignore these lines
  (require mzlib/defmacro)  ;

  ;; A function:
  (define (ready-to-eat? temp)
    (eq? temp 'just-right))

  ;; Call it in the REPL

 ----------------------------------------

Add a quote mark. Now it's just a quoted list (click Run),
and no function is defined to call

----------------------------------------

One more piece: quaisquote with a comma escape

  (let ([name 'ready-to-eat?])
   `(define (,name temp)
      (eq? temp 'just-right)))

----------------------------------------

Turn it into a function:

  (define (make-checker-defn name)
   `(define (,name temp)
      (eq? temp 'just-right)))

  (make-checker-defn 'ready-to-eat?)
  (make-checker-defn 'ready-to-sit?)

Still can't call theses functions...

----------------------------------------

  (defmacro make-checker-defn (name)
   `(define (,name temp)
      (eq? temp 'just-right)))

  (make-checker-defn ready-to-eat?)
  (make-checker-defn ready-to-sit?)

Works, and we'd call that `define-checker`, really:

  (defmacro define-checker (name)
   `(define (,name temp)
      (eq? temp 'just-right)))

  (define-checker ready-to-eat?)
  (define-checker ready-to-sit?)

;; call both

|#

(define (bear-face mood)
  (define hair 'curly-q)
  (case mood
   [(happier) (head #:hair hair #:mouth 'grin)]
   [(happiest) (head #:hair hair #:mouth 'big-grin #:eyes 'kinda-wide)]
   [(sortof-unhappy) (head #:hair hair #:mouth 'straight #:eyes 'worried)]
   [(unhappy) (head #:hair hair #:mouth 'grin #:eyes 'worried)]
   [(badly-embarassed) (head #:hair hair #:mouth 'frown #:eyes 'worried)]
   [(distraught) (head #:hair hair #:mouth 'frown #:eyes 'wide)]
   [else (error 'bear-face "unknown mood: ~e" mood)]))

(define (defmacro-to-phase-slides)
  (define s 3.5)
  (story-slides #:skip-intro? #t
                #:post-intro-title "Programming in PLT Scheme..."
                #:in-mzscheme-title "... in Macro-Extended Scheme"
                #:face (lambda (mood)
                         (scale (bear-face mood) s))
                #:face* (lambda (mood . more)
                          (scale (bear-face 'distraught) s)))

  (use-slides #:briefer? #t #:more-direct-deps? #t)
  (eval-when-slides #:briefer? #t
                    #:positive-phrasing? #t
                    #:run-time-note? #t)

  (phase-slides #:extras? #t))

(define (lex-scope-slides)
  (define end-steps
   (let* ([single (scale (make-racket-dots) 0.75)]
          [multi (scale (make-racket-tree) 0.75)]
          [mk (lambda (single? incidental? inevitable?)
                (define show-single (if single? values ghost))
                (define show-multi (if single? ghost values))
                (inset
                 (hb-append
                  (* 3 gap-size)
                  (blank)
                  (inset
                   (cb-superimpose (show-single single)
                                   (show-multi multi))
                   0 0 (* 3 gap-size) 0)
                  (inset
                   (vl-append
                    (* 2 gap-size)
                    ((if incidental? values ghost)
                     (lt-superimpose
                      (show-single (incidental "Macro details"))
                      (show-multi (incidental "AST representation"))))
                    ((if inevitable? values ghost)
                     (lt-superimpose
                      (show-single (inevitable "Phase distinction"))
                      (show-multi (inevitable "Macro-like scope")))))
                   0 gap-size 0 0))
                 0 (* 2 gap-size) 0 0))])
     (list
      (add-cite (blank)
                #:page (blank client-w (* 1.1 client-h))
                "Flatt [ICFP'02]")
      (mk #t #f #f)
      (mk #t #t #t)
      (mk #f #f #f)
      (mk #f #t #t))))
  
  (define end-steps-place (ghost (launder (apply ct-superimpose end-steps))))
  
  (define scheme (vc-append (current-line-sep)
                            (bt "Scheme")
                            (t "hygienic macros")))
  (define modules (vc-append (current-line-sep)
                             (bt "Modules")
                             (t "explicit phases")))
  
  (define (together scheme modules)
    (hc-append (* 2 gap-size)
               scheme
               (bt "+")
               modules))
  
  (define implies (inset (scale (bt "⇒") 1.2) 0 (* -3 gap-size) 0 0))
  
  (define (make-wizard hat)
    (let ([d 150])
      (inset (pict->pre-render-pict (inset (head #:hair hat #:mouth 'goatee) 0 d))
             0 (- d))))
  
  (define wizard (make-wizard 'wizard))
  (define wizard+ (make-wizard 'wizard+star))

  (define (content #:implies? [implies? #t]
                   #:wizard? [wizard? #f]
                   #:modules-n [modules-n 0]
                   #:end-step [end-step (blank)])
    (define g-scheme1 (ghost (launder scheme)))
    (define g-scheme2 (ghost (launder scheme)))
    (define (add-wizard p w)
      (if wizard?
          (refocus (vc-append (* 2 gap-size) p w)
                   p)
          p))
    (vc-append
     gap-size
     ((if implies? values ghost) implies)
     ((if implies? values ghost)
      (slide-pict (cb-superimpose g-scheme1
                                  (cellophane
                                   (together g-scheme2
                                             (add-wizard modules
                                                         wizard+))
                                   modules-n))
                  (add-wizard scheme wizard)
                  g-scheme1 g-scheme2
                  modules-n))
     (ct-superimpose end-steps-place
                     end-step)))
  
  (slide #:title use-lex-scope-balloon
         (content #:implies? #f))
  
  (slide #:title use-lex-scope-balloon
         (content))
   
  (play-n
   #:title use-lex-scope-balloon
   (lambda (n) (content #:modules-n n
                   #:wizard? #t
                   #:end-step (if (= n 1)
                                  (car end-steps)
                                  (blank)))))
  
  (for ([end-step (in-list (cdr end-steps))])
    (slide #:title use-lex-scope-balloon
           (content #:modules-n 1
                    #:end-step end-step))))
   

(define (module+phase-slides)
  (defmacro-to-phase-slides)
  (lex-scope-slides))

(module+ main
  (module+phase-slides))
