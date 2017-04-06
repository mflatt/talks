#lang slideshow
(require slideshow/code
         slideshow/balloon
         slideshow/play
         "config.rkt"
         "utils.rkt"
         "contributor.rkt"
         "turnstile.rkt"
         "cycle.rkt"
         "lesson.rkt")

(provide syntax-parse-slides
         
         loop-content)

(define (ex s) (bt s))
(define (pt s) (colorize (tt s) (current-base-color)))
(define (st s) (colorize (tt s) (current-literal-color)))
(define (ex-scale p) (scale p 0.75))

(define (make-eli-bear)
  (format-bear eli-bear "Eli" "Barzilay"))
(define (make-sam-bear)
  (format-bear sam-bear "Sam" "Tobin-Hochstadt"))
(define (make-robby-bear)
  (format-bear robby-bear "Robby" "Findler"))
(define (make-ryan-bear)
  (format-bear ryan-bear "Ryan" "Culpepper"))

(define (resp label now? make-bear dir)
  (if now?
      (refocus (pin-balloon (wrap-balloon (make-bear)
                                          dir 0 (if (eq? dir 'n)
                                                    (- gap-size)
                                                    gap-size))
                            label
                            label (if (eq? dir 'n) cb-find ct-find))
               label)
      label))

(define (maybe-add-cite cite p)
  (if cite (add-cite p cite) p))

(define (langs-slide #:eli-at? [eli-at? #f]
                     #:sam-match? [sam-match? #f]
                     #:robby-contracts? [robby-contracts? #f]
                     #:sam-tr? [sam-tr? #f])
  (slide
   #:title "Some Macro-Based (Sub-)Languages"
   (maybe-add-cite
    (cond
     [eli-at? "Barzilay [Scheme'09]"]
     [robby-contracts? "Findler and Felleisen [ICFP'02], Findler et al. [ECOOP'04], ..."]
     [sam-tr? "Tobin-Hochstadt and Felleisen [POPL'08], Tobin-Hochstadt et al. [PLDI'11], ..."]
     [else #f])
    (vc-append
     gap-size
     (blank)
     (table 2
            (list
             (ex "Scribble")
             (resp
              (ex-scale
               (hbl-append (pt "@") (code title) (pt "{") (st "Goldilocks and the Three Bears") (pt "}")))
              eli-at?
              make-eli-bear
              's)
             
             (resp (inset (ex "Match") 0 0 (* 3 gap-size) 0) sam-match? make-sam-bear 'n)
             (ex-scale
              (code (define (get-name c)
                      (match c
                        [(bear name size) name]
                        [(girl name hair-color) name]))))
             (resp (ex "Contracts") robby-contracts? make-robby-bear 's)
             (ex-scale
              (code (provide/contract
                     [get-name : (character? . -> . string?)])))
             (resp (ex "Typed Racket") sam-tr? make-sam-bear 'n)
             (ex-scale
              (code (: get-name : Character -> String)
                    (define (get-name c)
                      (match c
                        [(bear name size) name]
                        [(girl name hair-color) name])))))
            ltl-superimpose ltl-superimpose
            gap-size (* 2 gap-size))))))

(define (langs-slides)
  (langs-slide)
  (langs-slide #:eli-at? #t)
  (langs-slide #:sam-match? #t)
  (langs-slide #:robby-contracts? #t)
  (langs-slide #:sam-tr? #t))

;; ----------------------------------------
  
(define (example-slide #:ryan? [ryan? #f]
                       #:syntax-parse? [syntax-parse? #f]
                       #:not-id? [not-id? #f]
                       #:missing-colon? [missing-colon? #f]
                       #:impl? [impl? (or missing-colon? not-id?)])
  (slide
   #:title (if syntax-parse?
               (hbl-append (titlet "Pattern-Based Macros with ")
                           (resp (code syntax/parse)
                                 ryan?
                                 make-ryan-bear
                                 'n))
               "Pattern-Based Macros")
   (maybe-add-cite
    (and ryan? "Culpepper and Felleisen [ICFP'10]")
    (scale
     (code
      #,(cond
         [syntax-parse?
          (code
           (define-syntax with-checked
             (syntax-parser
               #:literals (:)
               [(_ ([var:id : pred rhs]
                    ...)
                   body ...)
                #'(let ([var (let ([v rhs])
                               (unless (pred v)
                                 (error 'id "bad" v))
                               v)]
                        ...)
                    body ...)])))]
         [else
          ((if impl? values ghost)
           (code
            (define-syntax with-checked
              (syntax-rules (:)
                [(_ ([var : pred rhs] ...)
                    body ...)
                 (let ([var (let ([v rhs])
                              (unless (pred v)
                                (error 'id "bad" v))
                              v)]
                       ...)
                   body ...)]))))])
      code:blank
      code:blank
      #,((if (and missing-colon? (not syntax-parse?))
             enpink
             values)
         (code
          (with-checked (#,(if missing-colon?
                               (code [x #,((if syntax-parse? enpink values) (code integer?)) 8])
                               (code [#,(if not-id? (enpink (code 0)) (code x)) : integer? 8]))
                         [y : string? "hello"])
            (string-append y (number->string x)))))
      code:blank
      #,(error-para
         #:width (* client-w 0.9)
         (cond
          [not-id?
           (if syntax-parse?
               "with-checked: expected identifier"
               "let: not an identifier")]
          [missing-colon?
           (if syntax-parse?
               "with-checked: expected the identifier `:'"
               "with-checked: bad syntax")]
          [else ""])))
     0.8))))

(define (syntax-parse-example-slides #:syntax-classes? [syntax-classes? #f])
  (example-slide)
  (example-slide #:impl? #t)
  (example-slide #:not-id? #t)
  (example-slide #:missing-colon? #t)
  (example-slide #:syntax-parse? #t)
  (example-slide #:syntax-parse? #t #:ryan? #t)
  (example-slide #:syntax-parse? #t #:not-id? #t)
  (example-slide #:syntax-parse? #t #:missing-colon? #t)

  (when syntax-classes?
    (slide
     #:title "Syntax Classes"
     (scale
      (code
       (begin-for-syntax
         (define-syntax-class clause
           #:literals (:)
           (pattern [var:id : pred rhs]
                    #:with check #'(unless (pred v)
                                     (error 'id "bad" v)))))
       code:blank
       (define-syntax with-checked
         (syntax-parser
           [(_ (c:clause ...)
               body ...)
            #'(let ([c.var (let ([v c.rhs])
                             c.check
                             v)]
                    ...)
                body ...)])))
      0.8))))

;; ----------------------------------------

(define (label-cycle p1 p2 p3)
  (define c (scale cycle 2))
  (define b 0)
  (inset
   (vc-append gap-size
              p1
              (refocus (hb-append (inset p3 0 0 0 b)
                                  c
                                  (inset p2 0 0 0 b))
                       c))
   0 (* -2 gap-size) 0 0))

(define (loop-content #:pl-n [pl-n 0] #:macros-t [macros-t bt])
  (label-cycle (fade-pict pl-n
                          (macros-t "Macros")
                          (bt "Programming Language"))
               (vc-append (bt "Abstractions")
                          (blank (current-line-sep))
                          (code syntax/parse)
                          (t "contracts")
                          (t "..."))
               (vc-append (bt "Tools")
                          (blank (current-line-sep))
                          (t "compiler")
                          (t "macro stepper")
                          (t "..."))))
  
(define (loop-slides)
  (define loop-title "Macros in the Loop")
  
  (play-n #:name loop-title
          (lambda (n) (loop-content #:pl-n n)))
  
  (slide
   #:name loop-title
   'alts~
   (let* ([p (loop-content #:pl-n 1)]
          [inc-body (vl-append
                     (current-line-sep)
                     (para #:fill? #f (code syntax/parse) "patterns")
                     (t "..."))]
          [inc (incidental inc-body
                           #:width (* client-w 1/2))]
          [inev (inevitable (lc-superimpose
                             (para #:fill? #f (bt "More") "than macros")
                             (blank 0 (pict-height inc-body))))]
          [mk (lambda (inev?)
                (refocus (vc-append gap-size
                                    p
                                    (ht-append
                                     (* 2 gap-size)
                                     inc
                                     ((if inev? values ghost) inev)))
                         p))])
     (list
      (list (mk #f))
      (list (mk #t))))))
                 
;; ----------------------------------------

(define (syntax-parse-slides)
  (langs-slides)
  (syntax-parse-example-slides)
  (turnstile-slides)
  (loop-slides))

(module+ main
  (syntax-parse-slides))
