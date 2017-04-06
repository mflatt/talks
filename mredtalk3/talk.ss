#lang slideshow

(require (lib "mred.ss" "mred")
         (lib "class.ss")
         (lib "math.ss")
         (lib "list.ss")
         (lib "etc.ss")
         (lib "step.ss" "slideshow")
         slideshow/code
         racket/runtime-path
         "../mredtalk2/modified.ss"
         "../mredtalk2/angel-sequence.ss"
         "../mredtalk2/demo-eval.ss"
         "accounting.ss"
         "ks.ss")

(provide mred-slides)

(define-runtime-path people "people")
(define-runtime-path mag.png "magnify.png")
(define-runtime-path mag2.png "magnify2.png")
(define-runtime-path web-break-png "browser.png" #;"web-break.png")
(define-runtime-path demo-rkt "../mredtalk2/demo.rkt")

(define (mred-slides)

(define title-slide? #f)
(define gnosys? #f)
(define gnosys-title? gnosys?)
(define os-title? #t)
(define chinese-name? #f)
(define edited-for-content? #f)
(define angel-slides? #t)
(use-pl-labels #t)
(define use-angel-wings? #f)
(define short-angel-sequence? #t)
(define process-facet-list-early? #f)
(define process-facet-list-late? #t)
(define non-drscheme-examples? #t)
(define compare-termination-languages? #f)
(define limit-memory-slide? #f)
(define accounting-section? #f)
(define kill-safe-section? #f)
(define fine-print? #f)
(define long-conclusion? #f)
(define no-conclusion? #t)
(define skip-icfp99-cite? #t)

(bitmap-draft-mode #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (author who where)
  (vc-append
   (current-line-sep)
   (colorize (bt who) "blue")
   (blank (/ (current-font-size) 3))
   (scale/improve-new-text
    (t where)
    0.8)))

(define PERSON-H (* 4 gap-size))

(define (add-picture who p)
  (let ([img (bitmap (build-path
                      people
                      (format "~a.jpg"
                              (car (regexp-match #rx"^[A-Z][a-z]+" who)))))])
    (vc-append (scale img (/ PERSON-H (pict-height img)))
               p)))

(define (authors whos where)
  (vc-append
   (current-line-sep)
   (scale
    (apply hc-append
           (* 2 gap-size)
           (for/list ([who (in-list whos)])
             (if (pict? who)
                 who
                 (add-picture who (bt who)))))
    0.8)
   (scale (t where) 0.8)))

(when title-slide?
  (slide
   (vc-append
    (current-line-sep)
    (cond
     [gnosys-title?
      (vc-append
       (current-line-sep)
       (titlet "GnoSys: Raising the Level of Discourse")
       (titlet "in Programming Systems"))]
     [os-title?
      (titlet "Programming Languages as Operating Systems")]
     [else
      (titlet "Processes without Partitions")]))
   (blank)
   (if gnosys-title?
       (blank)
       (bitmap (build-path (collection-path "icons") "PLT-206.png")))
   (blank)
   (if gnosys-title?
       (vc-append
        (* 2 gap-size)
        (authors '("Olin Shivers" "Matthias Felleisen" "Pete Manolios" "Mitch Wand")
                 "Northeastern University")
        (authors (list "Matt Might" (add-picture "Matthew" (colorize (bt "Matthew Flatt") "blue")))
                 "University of Utah"))
       (vc-append
        (current-line-sep)
        (author  (string-append "Matthew Flatt" 
                                (if chinese-name? "   马晓" ""))
                 "University of Utah")
        (blank)
        (blank)
        (scale/improve-new-text
         (let ([a (author "Adam Wick" "University of Utah")]
               [r (author "Robert Bruce Findler" "Northwestern University")])
           (hc-append
            (* 3 gap-size)
            (cc-superimpose (ghost r) a)
            (cc-superimpose (ghost a) r)))
         0.80)))))

(when edited-for-content?
  (modified-slide))

(use-angel-wings use-angel-wings?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mgt s) (colorize (bt s) "forestgreen"))

(define (spin p theta)
  (let ([d (make-pict-drawer p)]
        [w (pict-width p)]
        [h (pict-height p)])
    (let ([w2 (+ (abs (* w (cos theta))) (abs (* h (sin theta))))]
          [h2 (+ (abs (* h (cos theta))) (abs (* w (sin theta))))])
    (dc (lambda (dc x y)
          (let ([t (send dc get-transformation)])
            (send dc translate
                  (+ x (/ w 2) (/ (- w2 w) 2))
                  (+ y (/ h 2) (/ (- h2 h) 2)))
            (send dc rotate theta)
            (d dc (/ w -2) (/ h -2))
            (send dc set-transformation t)))
        w2
        h2))))


(define servlet
  (scale (vc-append
          (scheme-angel-file)
          (tt "servlet.rkt"))
         0.75))

(when gnosys?
 (with-steps
  (server analyze servlets contracts)
  (slide
   #:title "A GnoSys Application"
   (blank)
   (let ([contract-dircomm
          (scale
           (if (after? contracts)
               (refocus (vc-append (mgt "contracts")
                                   short-dircomm
                                   (mgt "processes"))
                        short-dircomm)
               short-dircomm)
           0.75)])
     (vc-append
      (let ([ws (vc-append (scheme-angel-file)
                           (tt "web-server.rkt"))]
            [lglass (bitmap mag2.png)]
            [rglass (bitmap mag.png)])
        (let ([l (inset (vr-append (inset (mgt "static analysis")
                                          0 0 (* 2 gap-size) 0)
                                   (scale lglass 1/2))
                        0 -50 -40 0)]
              [r (inset (vl-append (inset (mgt "theorem proving")
                                          (* 2 gap-size) 0 0 0)
                                   (scale rglass 1/2))
                        -40 -50 0 0)])
          (inset (hc-append ((vafter analyze) l)
                            (inset ws -10 30 -10 0) 
                            ((vafter analyze) r))
                 (- (pict-width l)) 0 (- (pict-width r)) 0)))
      ((vafter servlets)
       (hc-append (* 4 gap-size)
                  (vc-append (inset (spin contract-dircomm (* pi 3/8))
                                    60 0 0 0)
                             servlet)
                  (vc-append (inset (spin contract-dircomm (* pi -3/8))
                                    0 0 60 0)
                             servlet))))))))

(define gnosys-slide (most-recent-slide))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dim-color "dark gray")
(define hilite-color "black")
(define bright-color "blue") ; was "purple"

(define (half-page-item . s)
  (apply item #:width (* client-w 3/4) s))

(define (make-link s)
  (let ([p (t s)])
    (refocus (colorize
              (vc-append
               p
               (linewidth 2 (hline (pict-width p) 1)))
              "blue")
             p)))

(define outline
  (apply
   make-outline 
   'motivation "Motivation and Approach" #f
   'plt-scheme "Processes in Racket" 
   (lambda (sym)
     (vl-append
      (* 3 (current-line-sep))
      (half-page-item "Threads")
      (half-page-item "Parameters")
      (half-page-item "Eventspaces")
      (half-page-item "Custodians")))
   'accounting "Memory Accounting"
   (lambda (sym)
     (half-page-item "Without partitions" 
                     (blank (current-font-size))
                     (colorize (t "[ISMM 04]") dim-color)))
   (if kill-safe-section?
       (list
        'kill-safe "Synchronization Abstractions"
        (lambda (sym)
          (half-page-item "From thread-safe to kill-safe"
                          (blank (current-font-size))
                          (colorize (t "[PLDI 04]") dim-color))))
       null)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when angel-slides?
  (if short-angel-sequence?
      (if use-angel-wings?
          (simpler-angel-slides)
          (simplest-angel-slides))
      (angel-slides)))

(define (process-facet-list-slide)
  (slide
   #:title (if (use-pl-labels)
               "Process Concepts"
               "Languages as Operating Systems")
   (item "Threads")
   (item "Process-specific state (e.g., current directory)")
   (item "Graphical event loops")
   (item "Debugging capabilities")
   (item "Resource accounting")
   (item "Terminate a process and reclaim resources")))

(when process-facet-list-early?
  (process-facet-list-slide))
      
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mrt2-bitmap file)
  (bitmap (build-path (this-expression-source-directory)
                      'up
                      "mredtalk2"
                      file)))

(define (file-label p s)
  (vc-append
   (current-line-sep)
   p
   (bt s)))
   
(define prog-process-title "Process Examples")

#;
(when non-drscheme-examples?
  (slide
   #:title prog-process-title 
   (rb-superimpose
    (inset (bitmap "firefox.png") 0 0 (* gap-size 3) (* gap-size 4))
    (bitmap "terminal.png"))))

#;
(slide/title/center
 prog-process-title 
 (mrt2-bitmap "installer.bmp"))
 
(when non-drscheme-examples?
  (slide
   #:title prog-process-title   
   (bitmap web-break-png)))

(when non-drscheme-examples?
  (slide
   #:title prog-process-title
   (let ([ws (mrt2-bitmap "web-server.png")]
         [ie (mrt2-bitmap "ie.bmp")]
         [chrome (mrt2-bitmap "chrome.png")]
         [firefox (mrt2-bitmap "firefox.png")]
         [safari (mrt2-bitmap "safari.png")])
     (let ([ies (map launder (list ie chrome firefox safari))])
       (let ([p (vc-append
                 (* 2 (pict-height ws))
                 ws
                 (apply
                  hc-append
                  (* 2 (pict-width ie))
                  ies))])
         (foldl (lambda (ie p)
                  (pin-line p
                            ws cb-find
                            ie ct-find
                            #:line-width 1 
                            #:color dim-color))
                p ies))))))
 
(when #f
  (when non-drscheme-examples?
    (slide
     #:title prog-process-title
     (mrt2-bitmap "sirmail.bmp"))))

(define (mk-drs-layout s)
  (let* ([user-file (ghost (scheme-angel-file))]
         [main
          (vc-append
           (/ (current-font-size) 2)
           (hc-append
            (file-label (scheme-angel-file) "DrRacket")
            (ghost dircomm)
            (file-label user-file "user’s program"))
           (blank)
           together-arrows
           mred-logo)])
    (let-values ([(x y) (lt-find main user-file)])
      (pin-over main
                x y 
                (let* ([u (scheme-angel-file)]
                       [big (scale u s s)])
                  (inset big 
                         (/ (- (pict-width u) (pict-width big)) 2)
                         (/ (- (pict-height u) (pict-height big)) 2)))))))

(slide
 #:title prog-process-title
 (blank)
 (blank)
 (mk-drs-layout 1)
 (blank)
 (blank)
 (blank)
 (para
  (clickback
   (make-link "Run DrRacket")
   (lambda ()
     (end-subtalk)
     (begin-subtalk)
     (subtalk-eval
      `(begin
         (current-command-line-arguments (vector ,(path->string demo-rkt)))
         (exit-handler (lambda (x) (,end-subtalk)))
         (dynamic-require 'drracket #f)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(when process-facet-list-late?
  (process-facet-list-slide))
 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(define object-color "forest green")
(define (mk-object size)
  (cloud (* size 1.5 (current-font-size)) 
         (* size (current-font-size)) 
         object-color))

(define (mk-partition-icon objs)
  (frame (inset (hc-append 
                 (current-font-size)
                 (scheme-angel-file)
                 objs)
                (/ (current-font-size) 2))))

(define (connect-all p o . tos)
  (let loop ([p p][tos tos])
    (if (null? tos)
        p
        (loop
         (pin-line p o cb-find
                   (car tos) ct-find
                   #:line-width 1
                   #:color object-color)
         (cdr tos)))))

(define memory-icon
  (let* ([a1 (launder (scheme-angel-file))]
         [a2 (launder (scheme-angel-file))]
         [o1 (mk-object 1)]
         [o2 (mk-object 1.2)]
         [o3 (mk-object 1.3)]
         [o4 (mk-object 1)]
         [o5 (mk-object 1.4)]
         [o6 (mk-object 1.25)]
         [p (vc-append
             (current-font-size)
             (hc-append (* 0.5 (pict-width a1)) a1 a2)
             (blank)
             (hc-append (current-font-size) (ghost (launder o3)) o1 (blank) o2)
             (blank)
             (hc-append (current-font-size) o5 (ghost (launder o1)) o3 (ghost (launder o2)) o4 o6))])
    (connect-all (connect-all (connect-all (connect-all p a1 o1 o5) a2 o1 o2) o2 o6 o4) o1 o3)))

(define unix-memory-icon
  (hc-append
   (current-font-size)
   (mk-partition-icon (vc-append
                       (mk-object 2)
                       (hc-append (mk-object 1) (mk-object 1.2))))
   (mk-partition-icon (vc-append
                       (hc-append
                        (vc-append (mk-object 1) (mk-object 1.2))
                        (mk-object 2))
                       (mk-object 1.3)))))

(define (citation who)
  (colorize (t (format "[~a]" who)) dim-color))

(define (cite what who)
  (if who
      (hbl-append (bt what) (t "  ") (citation who))
      (bt what)))

(define term-title "Languages with Termination")

(define summary-figure
  (vc-append
   (current-font-size)
   (hc-append
    (scheme-angel-file)
    dircomm
    (scheme-angel-file))
   together-arrows
   mred-logo))
  
(when compare-termination-languages?
  (slide
   #:title term-title
   (table 2
          (list
           (cite "Pilot" "Redell80")
           (cite "SPIN" "Bershad95")
           (cite "JKernel" "Hawblitzel98")
           (cite "Alta" "Tullman99")
           (cite "KaffeOS" "Back00")
           (cite "JSR-121" "Soper03")
           (cite ".NET application domains" #f)
           (cite "Singularity" "Hunt07"))
          lbl-superimpose lbl-superimpose
          (* 4 gap-size) (* 2 gap-size))
   'next
   (blank)
   (blank)
   unix-memory-icon)

  (slide
   #:title term-title
   (cite "Racket" #f)
   (blank)
   (blank)
   memory-icon)

  (slide
   #:title "Processes"
   summary-figure
   (blank)
   (let ([w (* client-w 1/3)])
     (hc-append
      (* 2 gap-size)
      (para #:width w #:fill? #f #:align 'center
            "Processes without partitions")
      (scale (t "⇒") 2)
      (para #:width w #:fill? #f #:align 'center
            "Break ``process'' into multiple facets")))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(current-demo-directory (build-path (this-expression-source-directory)
                                    'up
                                    "mredtalk2"))

(define eval-link (scale (make-link "eval") 0.75))

(define aspect-layout #;'tall 'center)

(define aspectual
  (case-lambda
   [(s) (colorize (btitlet s) bright-color)]
   [(s a) (hbl-append (aspectual s)
                      (titlet " – ")
                      (titlet a))]))

(define (btitlet s)
  (text s (current-main-font) 40))

(define (aspect s)
  'nothing
  #;
  (colorize
   (para
    #:fill? #f
    ;; capitalize s:
    (it (string-append (string (char-upcase (string-ref s 0))) (substring s 1 (string-length s)))))
   "blue"))

(define (mk-code-eval code)
  (lambda ()
    (begin-subtalk)
    (subtalk-eval (read (open-input-string 
                         (format "(begin . ~s)" code))))))

(define-syntax-rule (eval-code-slide title . body)
  (demo-slide/title
   title
   (mk-code-eval (strip-unquote 'body))
   (scale (code . body) code-scale)))

(define-syntax (with-hilite stx)
  (syntax-case stx ()
    [(_ [id ...] expr)
     (let ([ids (map syntax-e (syntax->list #'(id ...)))])
       #`(code #,(let loop ([expr #'expr])
                   (cond
                    [(identifier? expr)
                     (if (memq (syntax-e expr) ids)
                         (datum->syntax expr
                                        `(unsyntax (hilite (code ,expr)))
                                        expr)
                         expr)]
                    [(syntax? expr)
                     (datum->syntax expr
                                    (loop (syntax-e expr))
                                    expr
                                    expr)]
                    [(pair? expr) (cons (loop (car expr))
                                        (loop (cdr expr)))]
                    [else expr]))))]))

(define (strip-unquote v)
  (cond
   [(not (pair? v)) v]
   [(eq? 'code:blank (car v))
    (strip-unquote (cdr v))]
   [(eq? 'unsyntax (car v))
    (let loop ([v (last v)])
      (cond
       [(eq? (car v) 'code)
        (strip-unquote (cadr v))]
       [(eq? 'with-hilite (car v))
        (strip-unquote (caddr v))]
       [else
        (loop (last v))]))]
   [else (cons (strip-unquote (car v))
               (strip-unquote (cdr v)))]))

(define code-scale 0.8)

(define-syntax-rule (eval-code-block/sep sep . body)
  (hbl-append
   sep
   (scale (code . body) code-scale)
   (clickback
    eval-link
    (mk-code-eval (strip-unquote 'body)))))

(define-syntax-rule (eval-code-block . body)
  (eval-code-block/sep (current-font-size) . body))

(define-syntax-rule (normal c)
  (parameterize ([code-colorize-enabled #t])
    c))

(define-syntax-rule (hilite c)
  (parameterize ([code-colorize-enabled #f])
    (colorize c bright-color)))

(define-syntax-rule (unhilite c)
  (parameterize ([code-colorize-enabled #f])
    (colorize c dim-color)))

(slide
 #:title (aspectual "Threads")
 #:layout aspect-layout
 #:inset demo-inset
 (aspect "concurrent execution")
 (blank)
 (vl-append
  (hbl-append
   (current-font-size)
   (scale
    (code
     (require "spin-display.rkt"))
    code-scale)
   (clickback
    eval-link
    (lambda ()
      (end-subtalk)
      (begin-subtalk)
      (subtalk-eval '(require "spin-display.scm")))))
  (eval-code-block
   code:blank
   (define (spin)
     (rotate-a-little)
     (sleep 0.1)
     (spin))
   code:blank
   (define spinner (#,(hilite (code thread)) spin)))
  (eval-code-block
   code:blank
   (#,(hilite (code kill-thread)) spinner))))

(slide
 #:title (aspectual "Parameters" "Thread-local State")
 #:layout aspect-layout
 #:inset demo-inset
 (aspect "thread-local state")
 (vl-append
  (current-line-sep)
  (eval-code-block
   (printf "Hello\n")
   (fprintf (#,(hilite (code current-output-port))) "Hola\n")
   (fprintf (#,(hilite (code current-error-port))) "Goodbye\n")
   (error "Ciao"))
  (eval-code-block/sep
   0
   code:blank
   #,(with-hilite
      [parameterize]
      (parameterize ([current-error-port (current-output-port)])
        (error "Au Revoir"))))
  (eval-code-block/sep
   0
   code:blank
   #,(with-hilite
      [parameterize thread]
      (parameterize ([current-error-port (current-output-port)])
        (thread
         (lambda ()
           (error "\u518D\u89C1"))))))))

(slide
 #:title (aspectual "Eventspaces" "Concurrent GUIs")
 #:layout aspect-layout
 #:inset demo-inset
 (aspect "concurrent GUIs")
 (blank)
 (vl-append
  (current-line-sep)
  (eval-code-block
   (thread (lambda () (message-box "One" "Hi")))
   (thread (lambda () (message-box "Two" "Bye"))))
  (eval-code-block
   code:blank
   code:blank
   (thread (lambda () (message-box "One" "Hi")))
   #,(with-hilite
      [current-eventspace make-eventspace]
      (parameterize ([current-eventspace (make-eventspace)])
        (thread (lambda () (message-box "Two" "Bye"))))))))

(slide
 #:title (aspectual "Custodians" "Termination and Clean-up")
 #:layout aspect-layout
 #:inset demo-inset
 (aspect "termination and clean-up")
 (blank)
 (vl-append
  (current-line-sep)
  (hbl-append
   (current-font-size)
   (scale
    (code
     (define c (#,(hilite (code make-custodian))))
     (parameterize ([#,(hilite (code current-custodian)) c])
       ....))
    code-scale)
   (clickback
    eval-link
    (lambda ()
      (end-subtalk)
      (begin-subtalk)
      (subtalk-eval '(define c (make-custodian)))
      (subtalk-eval '(parameterize ((current-custodian c))
                       (parameterize ((current-eventspace (make-eventspace)))
                         (dynamic-require "start-a-lot.scm" #f)))))))
  (eval-code-block
   code:blank
   (#,(hilite (code custodian-shutdown-all)) c))))

(when limit-memory-slide?
  (slide
   #:title (aspectual "Custodians" "Resource Limits")
   #:layout aspect-layout
   #:inset demo-inset
   (aspect "resource limits")
   (blank)
   (vl-append
    (current-line-sep)
    (hbl-append
     (current-font-size)
     (scale
      (code
       (define (run-away)
         (cons 1 (run-away)))
       code:blank
       (#,(hilite (code custodian-limit-memory)) c 2000000)
       code:blank
       (parameterize ([current-custodian c])
         ....
         (thread run-away)))
      code-scale)
     (clickback
      eval-link
      (lambda ()
        (end-subtalk)
        (begin-subtalk)
        (subtalk-eval '(define c (make-custodian)))
        (subtalk-eval '(begin
                         (custodian-limit-memory c 2000000)
                         (parameterize ([current-custodian c])
                           (parameterize ([current-eventspace (make-eventspace)])
                             (dynamic-require "start-a-lot.scm" #f)
                             (thread
                              (lambda ()
                                (let loop () (cons (loop)))))))))))))))

(define (demo-slide/title s thunk . x)
  (slide
   #:title s
   #:inset demo-inset
   (lbl-superimpose
    (cc-superimpose
     (apply-slide-inset demo-inset titleless-page)
     (apply
      vl-append
      (current-font-size)
      x))
    (clickback eval-link thunk))))

(slide
 #:title "Building a Programming Environment"
 (para #:fill? #f
       (clickback
        (make-link "RacketEsq")
        (lambda ()
          (end-subtalk)
          (begin-subtalk)
          (subtalk-eval '(require "racket-esq.rkt"))))
       ", a mini DrRacket"
       (blank (current-font-size))
       (if skip-icfp99-cite? "" (colorize (t "[ICFP 99]") dim-color))))

(demo-slide/title
 "GUI – Frame"
 (lambda ()
   (end-subtalk)
   (begin-subtalk)
   (subtalk-eval 
    '(begin 
       (define frame (new frame% [label "RacketEsq"] [width 400] [height 185]))
       (send frame show #t))))
 (scale
  (code
   (define frame
     (new #,(hilite (code frame%))
          [label "RacketEsq"]
          [width 400]
          [height 175]))
   code:blank
   (send frame show #t))
  code-scale))

(eval-code-slide
 "GUI – Reset Button"
 (new #,(hilite (code button%))
      [label "Reset"]
      [parent frame]
      [callback (lambda (b e) 
                  (reset-program))]))

(eval-code-slide
 "GUI – Interaction Area"
 (define repl-display-canvas
   (new #,(hilite (code editor-canvas%))
        [parent frame])))

(demo-slide/title
 "GUI – Interaction Buffer"
 (lambda ()
   (subtalk-eval '(begin
                    (load "text.ss")
                    (define repl-editor (make-object esq-text%))
                    (send repl-display-canvas set-editor repl-editor))))
 (scale
  (code
   (define esq-text%
     (class #,(hilite (code text%)) .... (evaluate str) ....))
   code:blank
   (define repl-editor (new esq-text%))
   (send repl-display-canvas set-editor repl-editor))
  code-scale))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-code-slide
 "Evaluator"
 #,(with-hilite
    [thread read eval print]
    (define (evaluate expr-str)
      (thread
       (lambda ()
         (print (eval (read (open-input-string expr-str))))
         (newline)
         (send repl-editor new-prompt))))))

(demo-slide/title
 "Evaluator Output"
 (lambda ()
   (subtalk-eval
    '(begin
       (define user-output-port
         (make-output-port
          'stdout
          always-evt
          (lambda (s start end nonblock? w/break?) 
            (send repl-editor output (bytes->string/utf-8 (subbytes s start end)))
            (- end start))
          void))

       (define (evaluate expr-str)
         (thread
          (lambda ()
            (current-output-port user-output-port)
            (with-handlers ((exn? 
                             (lambda (exn)
                               (display (exn-message exn)))))
              (print (eval (read (open-input-string expr-str)))))
            (newline)
            (send repl-editor new-prompt)))))))
 (scale
  (code
   (define user-output-port
     (make-output-port .... repl-editor ....))
   code:blank
   #,(unhilite
      (code
       (define (evaluate expr-str)
         #,(normal
            (code
             (parameterize ([current-output-port user-output-port])
               #,(unhilite
                  (code
                   (thread
                    (lambda ()
                      ....)))))))))))
  code-scale))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(demo-slide/title
 "Evaluating GUIs"
 (lambda ()
   (subtalk-eval
    '(begin
       (define user-eventspace (make-eventspace))
       
       (define (evaluate expr-str)
         (thread
          (lambda ()
            (current-output-port user-output-port)
            (current-eventspace user-eventspace)
            (with-handlers ((exn? 
                             (lambda (exn)
                               (display (exn-message exn)))))
              (print (eval (read (open-input-string expr-str)))))
            (newline)
            (send repl-editor new-prompt)))))))
 (scale
  (code
   (define user-eventspace (make-eventspace))
   code:blank
   #,(unhilite
      (code
       (define (evaluate expr-str)
         (parameterize ([current-output-port user-output-port]
                        #,(normal
                           (code
                            [current-eventspace user-eventspace])))
           (thread
            (lambda ()
              ....)))))))
  code-scale))

(demo-slide/title
 "Custodian for Evaluation"
 (lambda ()
   (subtalk-eval
    '(begin
       (define user-custodian (make-custodian))
       (define user-eventspace
         (parameterize ((current-custodian user-custodian))
           (make-eventspace)))
       (define (evaluate expr-str)
         (parameterize ((current-custodian user-custodian)
                        (current-eventspace user-eventspace))
           (queue-callback
            (lambda ()
              (current-output-port user-output-port)
              (with-handlers ((exn? 
                               (lambda (exn)
                                 (display (exn-message exn)))))
                (print (eval (read (open-input-string expr-str)))))
              (newline)
              (send repl-editor new-prompt))))))))
 (scale
  (code
   (define user-custodian (make-custodian))
   code:blank
   (define user-eventspace
     (parameterize ([current-custodian user-custodian])
       (make-eventspace)))
   code:blank
   #,(unhilite
      (code
       (define (evaluate expr-str)
         (parameterize ([current-output-port user-output-port]
                        [current-eventspace user-eventspace]
                        #,(normal
                           (code
                            [current-custodian user-custodian])))
           (thread
            (lambda ()
              ....)))))))
  code-scale))

(eval-code-slide
 "Reset Evaluation"
 (define (reset-program)
   (custodian-shutdown-all user-custodian)
   code:blank
   (set! user-custodian (make-custodian))
   (parameterize ((current-custodian user-custodian))
     (set! user-eventspace (make-eventspace))
     (send repl-editor reset))))

#;
(eval-code-slide
 "Limit Memory Use"
 #,(unhilite
    (code
     (define (reset-program)
       (custodian-shutdown-all user-custodian)
       code:blank
       (set! user-custodian (make-custodian))
       #,(normal (code (custodian-limit-memory user-custodian 1000000)))
       (parameterize ((current-custodian user-custodian))
         (set! user-eventspace (make-eventspace))
         (send repl-editor reset))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (when fine-print?
   (parameterize ([current-para-width (* 0.9 client-w)])
     (slide
      #:title "Fine Print"
      (item "Kill-safe synchronization requires programmer effort" (citation "PLDI 04"))
      (item "Reachability-based memory accounting needs hierarchy" (citation "ISMM 04"))
      (item "Continuations need to be delimited" (citation "ICFP 07"))
      (item "Contracts need specific runtime support" (citation "in submission"))
      #;(item "A safe-for-space runtime implementation matters" (citation "ongoing"))
      (item "Safe language extension needs permissions on syntax" (citation "ongoing")))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(require "../killsafetalk/squiggle.ss")

(define (scale-process p) (scale p 4))

(slide/title/center
 "Recap: Unix"
 (scale-process unix))

(slide/title/center
 "Recap: Lisp Machine"
 (scale-process jvm))

(slide/title/center
 "Recap: Racket"
 (scale-process mz))

|#

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when accounting-section?

  (outline 'accounting)

  (for-each
   (lambda (s)
     (slide
      #:title "Resource Consumption"
      (mk-drs-layout s)))
   '(1 1.25 1.5 2 3 4))

  (void
   (parameterize ([current-para-width (* client-w 0.9)])
     (with-steps (unix mz gc)
                 (slide
                  #:title "Resource Accounting"
                  (cc-superimpose
                   ((vonly unix)
                    (item (colorize (bt "Conventional OS") "red") ": process memory use = size of partition"))
                   ((vonly mz)
                    (item (colorize (bt "Language as OS") "blue") ": process memory use = size of owned data"))
                   ((vonly gc)
                    (para "Our strategy: compute accounting charges during GC")))
                  (blank)
                  (cc-superimpose
                   ((vonly unix)
                    unix-memory-icon)
                   ((vafter mz) memory-icon))
                  (blank)
                  (cc-superimpose
                   ((vonly unix)
                    (vc-append
                     gap-size
                     (subitem "Accounting is easy")
                     (subitem "Trading data is difficult")))
                   ((vonly mz)
                    (vc-append
                     gap-size
                     (subitem "Trading data is easy")
                     (subitem "Accounting" (it "appears") "difficult: sharing, real-time tracking")))
                   ((vonly gc)
                    (vc-append
                     gap-size
                     (para "See also [Price03]")
                     #;
                     (colorize
                      (para* "Exact accounting is"
                             (hbl-append (t "O(N") (text "2" `(superscript . , main-font) font-size) (t ")"))
                             "in the worst case...")
                      "red")))))))))

(define ((owner +a?) s)
  (if (memq s '(z s))
      `(,(if +a? "B, A" "B") ,(symbol->string s) #f)
      `("A" ,(symbol->string s) #t)))

(when accounting-section?
  (void
   (with-steps (unknown known link)
               (slide
                #:title "Basic Accounting"
                (make-basic-picture 
                 '(A B) 
                 '(1 2 3)
                 `(,(map (before known values (owner (after? link))) '(x y z))
                   ,(map (before known values (owner (after? link))) '(q r s)))
                 `((A 1) (A 2) (B 3) 
                   ,@(after link '((B A)) null) 
                   (1 x) (2 y) (3 z) (x q) (y r) (z s)))))))

(define (mk-sharing-picture known? child?)
  (make-basic-picture 
   '(A B)
   '(1 2) 
   (if known?
       `((["A" "x" #t] 
          ["B" "y" #f])
         ([,(if child? "A" "A or B") "z" #t]))
       '((x y) (z)))
   `((A 1) (B 2) (1 x) (2 y) (x z) (y z)
     ,@(if child? '((B A)) '()))))

(when accounting-section?
  (void
   (with-steps (unknown both child)
               (slide
                #:title (after child "Sharing: Charge the Parent" "Sharing")
                (mk-sharing-picture (after? both) (after? child)))))

  (void
   (with-steps (unknown known cust weak)
               (slide
                #:title "Threads, Custodians, and Weak References"
                (make-basic-picture 
                 '(A B) 
                 '(1 2) 
                 (before known '((x)) '((("B" "x" #t))))
                 `((A 1) (B 2) ,(after weak `(1 x !) `(1 ,(after cust 'B 2))) (2 x))))))

  (slide
   #:title "Why Charge the Parent?"
   (blank)
   (blank)
   (mk-sharing-picture #t #t)
   (blank)
   (blank)
   (blank)
   (blank)
   'next
   (item "Parent is responsible for children")
   'next
   (item "Children refer to parent, so if the parent refers to children data directly,"
         " any child is charged for all children")))

(define (full-strikeout image)
  (let ([w (pict-width image)]
        [h (pict-height image)])
    (dc (lambda (dc dx dy)
          (let ([pen (send dc get-pen)]
                [pictdraw (make-pict-drawer image)])
            (pictdraw dc dx dy)
            
            (send dc set-pen (make-object pen% (make-object color% "Red")
                                          10 'solid))
            (send dc draw-line dx dy (+ w dx) (+ h dy))
            (send dc draw-line (+ dx w) dy dx (+ dy h))
            
            (send dc set-pen pen)))
        w h 0 0)))

(define (strikeout image)
  (let ([w (pict-width image)]
        [h (pict-height image)])
    (cb-superimpose
     image
     (inset (full-strikeout (blank w (* 2/5 h))) 0 0 0 (* 1/10 h)))))

(define dscheme (scale (bitmap "loop.png") 0.45))
(define dscheme-busy (scale (bitmap "deep.png") 0.45))

(define running-lbl (colorize (bt "Bad Loop") "Red"))
(define normal-lbl (colorize (bt "Normal") "Blue"))
(define killed-lbl (colorize (bt "Shut Down") "Red"))

(define (drs-slide init? so1 so2 l1 l3)
  (slide
   #:title (string-append (if init? "Initial " "Current ") "Experience: DrRacket")
   (hc-append 40 
              (vc-append 20 (so1 dscheme-busy) l1)
              (vc-append 20 dscheme normal-lbl)
              (vc-append 20 (so2 dscheme) l3))))

(when accounting-section?
  (drs-slide #t values values running-lbl normal-lbl)
  (drs-slide #t values strikeout running-lbl killed-lbl))

(define (inset-i p) (inset p 20 0 0 0))

(when accounting-section?
  (void
   (with-steps (strong weak explain)
               (slide
                #:title (before weak "DrRacket Bug" "DrRacket Repair")
                (blank)
                (make-basic-picture 
                 '(DrRacket User1 User2)
                 '(0 1 2)
                 '((x _ _)
                   (_ y z))
                 `((DrRacket 0) 
                   (User1 1) 
                   (User2 2)
                   (0 x) (1 x) (2 x)
                   (1 y) (2 z)
                   (x y ,@(after weak '(!) '())) 
                   (x z ,@(after weak '(!) '()))))
                (blank)
                ((vafter explain)
                 (vl-append
                  (* 2 (current-line-sep))
                  (para #:fill? #f "Changed 5 references:")
                  (inset-i (item #:fill? #f "Weakened 2"))
                  (inset-i (item #:fill? #f "Removed 2"))
                  (inset-i (item #:fill? #f "Moved 1 into child")))))))

  (drs-slide #f values values running-lbl normal-lbl)
  (drs-slide #f strikeout values killed-lbl normal-lbl)
  
  (slide
   #:title "Accounting without Partitions"
   (vl-append
    gap-size
    (para #:fill? #f "Useful accounting")
    (item #:fill? #f "Doesn't need partitions")
    (item #:fill? #f "Does need hierarchy"))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when kill-safe-section?
  (outline 'kill-safe))
(when kill-safe-section?
  (kill-safe-slides))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when gnosys?
  (re-slide gnosys-slide)

  (slide
   #:title "Wireless Router"
   (hc-append (bitmap "netgear.png")
              (bt " + ")
              (t "DD-WRT")
              (bt " + ")
              (t "Racket"))
   (vl-append
    gap-size
    (item #:fill? #f "Racket-based web server for configuration")
    (item #:fill? #f "Racket-based DNS server"))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless no-conclusion?
  (slide
   #:title "Conclusion"
   ((if long-conclusion? (lambda (p) (scale p 0.6)) values)
    summary-figure)
   (if long-conclusion?
       (vc-append
        gap-size
        (item "Programmers need OS-like constructs in languages")
        (vc-append
         (* 4 (current-line-sep))
         (subitem "concurrency")
         (subitem "adjust run-time environment")
         (subitem "easy termination"))
        (blank)
        (item "Multiple language constructs for \u201Cprocess\u201D")
        (subitem "programmer can mix and match to"
                 "balance isolation and cooperation"))
       (vc-append
        (* 2 (current-line-sep))
        (para "But don\u2019t partition data:")
        (subitem "closures")
        (subitem "objects")
        (subitem "continuations")
        (subitem "...")))))

)

(module+ main
  (mred-slides))
