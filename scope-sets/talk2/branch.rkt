#lang slideshow

(provide branches-slide)

(define footnote-color "steelblue")

(define (supert s)
  (colorize
   (text s `(superscript . ,(current-main-font)) (current-font-size))
   footnote-color))

(define (fn p s)
  (refocus (hbl-append p (supert s))
           p))

(define (lt s) (t s))
(define (dt s) (it s))

(define (branches-slide)
  (slide
   #:title "Explore More"
   (colorize (tt "https://github.com/mflatt/expander") "blue")
   (blank)
   (vr-append
    gap-size
    (table
     3
     (list
      (lt "branch") (lt "description")              (fn  (lt "LoC") "*")
      (tt "pico")   (dt "We just built it")             (tt "~250")
      (tt "nano")   (dt "Implicit quote and multi-arg λ") (tt "~300")
      (tt "micro")  (dt "Split into modules")           (tt "~700")
      (tt "mini")   (dt "Definition contexts")        (tt "~1,300")
      (tt "demi")   (dt "Modules & phases")           (tt "~3,000")
      (tt "master") (dt "Full Racket expander")  (fn (tt "~20,000") "†"))
     (list* lbl-superimpose lbl-superimpose rbl-superimpose)
     lbl-superimpose
     (* 2 gap-size) (cons (* 4 (current-line-sep))
                          (* 2 (current-line-sep))))
    (colorize
     (vr-append
      (current-line-sep)
      (para #:fill? #f (scale (t "* without examples/tests") 0.8))
      (para #:fill? #f (scale (t "† without bootstrap & extract") 0.8)))
     footnote-color))
   (let ([p (colorize (rotate (text "Thanks!" '(italic . "SignPainter") 128) (/ pi 16))
                      "firebrick")])
     (inset p 0 (* -2/5 (pict-height p)) (* 1.5 (pict-width p)) 0))))
  
(module+ main
  (branches-slide))

