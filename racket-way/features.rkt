#lang slideshow
(require "../scribble/talk/movie.ss"
         "st.rkt"
         slideshow/code
         racket/runtime-path)

(provide features-slides
         example-racket-program
         ide-screenshot)

(define-runtime-path battery-dir "batteries")
(define-runtime-path drracket+shell-png "drracket+shell.png")

(define battery (bitmap (build-path battery-dir "battery.png")))
(define batteries
  (for/list ([i (in-range 9)]) (bitmap (build-path
                                        battery-dir
                                        (format "battery~a.png" i)))))

(define labels '(("data" "structures")
                 ("cont-" "inuations")
                 ("modules")
                 ("classes")
                 ("doc" "tools")
                 ("contracts")
                 ("regexps")
                 ("compre-" "hensions")
                 ("exception" "handling")))

(define (add-label b i)
  (cb-superimpose
   b
   (cc-superimpose
    (inset
     (scale
      (rotate (apply vl-append
                     (map st (list-ref labels i)))
              (/ pi 2))
      0.8)
     0 0 10 10)
    (blank 1 (* 2/3 (pict-height b))))))

(define example-racket-program
  (scale
   (code #,(tt "#lang") racket
         code:blank
         (code:comment "Grep stdin for \"Racket\":")
         (for ([line (in-lines)])
           (when (regexp-match? #rx"Racket" line)
             (displayln line))))
   0.75))

(define ide-screenshot
  (scale (bitmap drracket+shell-png) 0.75))

(define (features-slide step? battery-step timeout)
  (slide
   #:title (hc-append gap-size
                      (scale plt-bm 0.33)
                      (titlet "Racket"))
   #:timeout timeout
   (item "A dialect of Lisp and a descendant of Scheme")
   (blank)
   'alts
   ((if step? list just-second)
    (list
     example-racket-program)
    (list
     (item "Optimizing bytecode+JIT compiler")
     (blank)
     'alts
     ((if step? list just-second)
      (list
       (hc-append
        (* 2 gap-size)
        (code ....
              (+ x y)
              ....)
        (scale (t "⇒") 2)
        (tt* "...."
             "test $0x1, eax"
             "je   not_fixnum_plus"
             "test $0x1, ecx"
             "je   not_fixnum_plus"
             "xor  $0x1, eax"
             "add  ecx, eax"
             "...."))
       (parameterize ([current-font-size (floor (* #e0.8 (current-font-size)))])
         (colorize (para #:align 'right "performance comparable to Clojure, Go, OCaml")
                   "forestgreen")))
      (list
       (item "Command-line tools plus DrRacket IDE")
       (blank)
       'alts
       ((if step? list just-second)
        (list
         ide-screenshot)
        (list
         (item "Batteries included")
         (let ([get-b 
                (lambda (plain?)
                  (apply hc-append
                         -12
                         (for/list ([i (in-range 9)])
                           (inset (if plain?
                                      (add-label battery i)
                                      ((if (and battery-step
                                                (i . <= . battery-step))
                                           values
                                           ghost)
                                       (list-ref batteries i)))
                                  0 (* 5 i) 0 0))))])
           (lt-superimpose (get-b #t)
                           (inset (get-b #f) 10 42 0 0)))))))))))

(define (just-second a b) (list b))

(define (features-slides)
  (features-slide #t #f #f)
  (define len (length batteries))
  (for ([i (sub1 len)])
    (features-slide #f i 0.25))
  (features-slide #f (sub1 len) #f))

(module+ main
  (features-slides))
