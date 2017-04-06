#lang at-exp slideshow
(require slideshow/code
         slideshow/balloon
         "style.rkt"
         "bubble.rkt"
         "in-file.rkt")

(provide doc-slides
         short-doc-slides)

(define (scribble-color . strs)
  (apply
   vl-append
   (current-code-line-sep)
   (for/list ([str (in-list strs)])
     (scribble-color-one str))))

(define (scribble-color-one str)
  (cond
   [(list? str)
    (define r (scribble-color-one
               (apply string-append
                      (for/list ([s (in-list str)])
                        (if (string? s)
                            s
                            ".")))))
    (define cw (pict-width (tt "x")))
    (define-values (p leftover)
      (for/fold ([p (tt "")] [r r]) ([s (in-list str)])
        (if (string? s)
            (values (hbl-append p (clip (inset r
                                               0 
                                               0 
                                               (- (* cw (string-length s))
                                                  (pict-width r))
                                               0)))
                    (inset r (- (* cw (string-length s))) 0 0 0))
            (values (hbl-append p s)
                    (inset r (- cw) 0 0 0)))))
    p]
   [(regexp-match #rx"^(.*)@([a-zA-Z-]*)(.*)" str)
    => (lambda (m)
         (hbl-append (scribble-color-one (cadr m))
                     (colorize (tt "@") (current-base-color))
                     (colorize (tt (caddr m)) (current-id-color))
                     (scribble-color-one (cadddr m))))]
   [(regexp-match #rx"^(.*)@(.*)" str)
    => (lambda (m)
         (hbl-append (scribble-color-one (cadr m))
                     (colorize (tt "@") (current-base-color))
                     (scribble-color-one (caddr m))))]
   [(regexp-match #rx"^(.*)[[]([^]]*)[]](.*)" str)
    => (lambda (m)
         (hbl-append (scribble-color-one (cadr m))
                     (colorize (tt "[") (current-base-color))
                     (colorize (tt (caddr m)) (current-id-color))
                     (colorize (tt "]") (current-base-color))
                     (scribble-color-one (cadddr m))))]
   [(regexp-match #rx"^(.*)([{}])(.*)" str)
    => (lambda (m)
         (hbl-append (scribble-color-one (cadr m))
                     (colorize (tt (caddr m)) (current-base-color))
                     (scribble-color-one (cadddr m))))]
   [else
    (colorize (tt str) (current-literal-color))]))
  
(define defin-code (code define))
(define for-label-code (code for-label))
(define racket-code (code racket))

(define (doc-slides)
  (define (for-label-slide #:for-label? [for-label? #f]
                           #:racket-define? [racket-define? #f]
                           #:racket-form? [racket-form? #f])
    (slide
     #:title "Documentation"
     (scale
      (let* ([for-label-code
              (if for-label? 
                  (encloud for-label-code #:color "yellow") 
                  for-label-code)]
             [p
              (code
               #,(tt "#lang") scribble/manual
               #,(htl-append
                  (scribble-color "@")
                  (code (require (#,for-label-code lang/htdp-beginner))))
               code:blank
               #,(let-syntax ([define (make-code-transformer #'defin-code)])
                   (htl-append
                    (scribble-color "@")
                    (code
                     (define (step n)
                       #,(hbl-append
                          @scribble-color|{@section{Step @}|(code (format "~a" n))
                          @scribble-color|{}}|)))))
                 code:blank
                 @#,scribble-color|{@step[1]}|
                 code:blank
                 @#,scribble-color|{Define the function @racket[seconds->days]}|
                 @#,scribble-color|{using @racket[define].}|
                 code:blank
                 @#,scribble-color|{@step[2]}|
                 code:blank
                 @#,scribble-color|{Define the function @racket[current-days]}|
                 #,(hbl-append @scribble-color|{using @}| racket-code @scribble-color|{[define].}|))]
             [p (if racket-define?
                    (pin-balloon
                     (wrap-balloon
                      (para #:fill? #f
                            "normal Racket" (code define))
                      'sw 0 (* 5 gap-size))
                     p defin-code ct-find)
                    p)]
             [p (if racket-form?
                    (pin-balloon
                     (wrap-balloon
                      (para #:fill? #f
                            #:width (/ client-w 2)
                            "Form that hyperlinks code based on" (code for-label) "bindings")
                      'nw 0 (- gap-size))
                     p racket-code cb-find)
                    p)])
        p)
      0.8)))
  (for-label-slide)
  (for-label-slide #:for-label? #t)
  (for-label-slide #:for-label? #t #:racket-define? #t)
  (for-label-slide #:for-label? #t #:racket-define? #t #:racket-form? #t)

  (slide
   #:title "Docstrings"
   (scale
    (code
     (define current-seconds
       (lambda ()
         "reports the time in seconds since the Epoch"
         ....))
     code:blank
     (define current-days
       (lambda ()
         "reports the time in days since the Epoch"
         ....))
     code:blank
     (define current-years
       (lambda ()
         "reports the time in years since the Epoch"
         ....)))
    0.8))
  
  (slide
   #:title "Abstraction Over Docstrings"
   (scale
    (code
     (define (docs-for-current what)
       (format "reports the time in ~a since the Epoch"
               what))
     code:blank
     (define current-seconds
       (lambda ()
         (docs-for-current "seconds")
         ....))
     code:blank
     (define current-days
       (lambda ()
         (docs-for-current "days")
         ....))
     code:blank
     (define current-years
       (lambda ()
         (docs-for-current "years")
         ....)))
    0.8))

#;
  (slide
   #:title "Docstrings"
   (code
    (define (docs-for-current what)
      (format "reports the time in ~a since the Epoch"
              what))
    code:blank
    (define current-seconds
      (lambda ()
        #:doc (docs-for-current "seconds")
        ....))))

  (slide
   #:title "Rackety Docstrings"
   (mk-file #:name "program"
            #:suffix "rkt"
            (scale
             (code
              (begin-for-doc
               (define (docs-for-current what)
                 (format 
                  "reports the time in ~a since the Epoch"
                  what)))
              code:blank
              (define current-seconds
                (lambda ()
                  #:doc (docs-for-current "seconds")
                  ....)))
             0.8)))

  (slide
   #:title "Rackety Docstrings"
   (mk-file #:name "program"
            #:suffix "rkt"
            (scale
             (code
              (module+ doc
                (define (docs-for-current what)
                  (format 
                   "reports the time in ~a since the Epoch"
                   what)))
              code:blank
              (define current-seconds
                (lambda ()
                  ....))
              code:blank
              (module+ doc
                (provide current-seconds)
                (define current-seconds 
                  (docs-for-current "seconds"))))
             0.8)))

  ((make-in-source-doc-slides)))

(define (short-doc-slides)
  (define s (make-in-source-doc-slides))
  (define un (/ 1 0.8))

  (s)
  (s #:hilite-doc (compose (bubble #:scale un 'n "documentation-time code") (hi "yellow")))
  (s #:hilite-doc (hi extra-color)
     #:hilite-ref (hi ref-color)
     #:hilite-a-ref (compose (bubble #:scale un 's "also referenced at documentation-time")
                             (hi ref-color)))
  (s #:hilite-doc (hi extra-color)
     #:hilite-ref (hi ref-color)))

(define (make-in-source-doc-slides)
  (define (seconds->hours-docs ref)
    (define-syntax-rule (mk s id t)
      (list s (ref (code id)) t))
    (scribble-color
     (mk "@{Takes @racket[" secs "], a number of seconds")
     "  since the Epoch, and converts it to a number"
     "  of days since the Epoch, rounding down."
     ""
     "  For example, compose with the"
     (mk "  @racket[" current-seconds "] function to get")
     (mk "  @racket[" current-hours "].}")))

  (define (doc-slide #:hilite-doc [hilite-doc values]
                     #:hilite-ref [ref values]
                     #:hilite-a-ref [a-ref ref])
    (slide
     #:title "In-Source Documentation"
     (mk-file #:name "time"
              #:suffix "rkt"
              (scale
               (code
                code:blank
                (define (#,(a-ref (code seconds->hours)) #,(ref (code secs)))
                  #:contract (#,(ref (code exact-integer?)) . -> . #,(ref (code exact-integer?)))
                  #:docs #,(hilite-doc (seconds->hours-docs ref))
                  (quotient secs (* 60 60)))
                code:blank)
               0.8))))

  doc-slide)

(module+ main
  (short-doc-slides))
