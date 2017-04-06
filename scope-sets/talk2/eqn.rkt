#lang slideshow
(require "code.rkt")

(provide reduction-slides)

(define (subs p n)
  (hbl-append p (text (number->string n) `(subscript . ,(current-main-font))
                      (current-font-size))))

(define (make-reduction-slide #:sos-rhs? [sos-rhs? #f]
                              #:sos-lhs? [sos-lhs? sos-rhs?]
                              #:rs-rhs? [rs-rhs? sos-lhs?])
  (slide
   (parameterize (#;[current-main-font 'roman])
     (define e_1 (subs (it "e") 1))
     (define e_2 (subs (it "e") 2))
     (define x_1 (subs (it "x") 1))
     (define x_2 (subs (it "x") 2))
     (define x_3 (colorize (subs (it "x") 3) "red"))
     (define <- (t "←"))
     (define <~ (t "↜"))
     (define (lam x e) (hbl-append 2 (t "(λ") (hbl-append x (t ".")) (hbl-append e (t ")"))))
     (define (sub e x v #:<- [<- <-]) (hbl-append e (t "[") x <- v (t "]")))
     (define (not-in e s) (hbl-append 2 e (t "∉") s))
     (define (union a b) (hbl-append 2 a (t "∪") b))
     (define (minus a b) (hbl-append 2 a (t "\\") b))
     (define (set c) (hbl-append (t "{") c (t "}")))
     (define (FV e) (hbl-append (it "FV") (t "(") e (t ")")))
     (define (w p) (inset p 2 0))
     (define (V ? p) (if ? p (ghost p)))
     (scale
      (vc-append
       (* 4 gap-size)
       (vr-append
        (current-line-sep)
        (hbl-append gap-size
                    (sub (lam x_1 e_1) x_2 e_2)
                    (V rs-rhs? (t "="))
                    (V rs-rhs? (lam x_3 (sub (sub e_1 x_1 x_3) x_2 e_2))))
        (V rs-rhs?
           (hbl-append (t "where ") (not-in x_3 (minus (union (union (FV e_1) (FV e_2)) (set x_2)) (set x_1))))))
       (V sos-lhs?
          (vr-append
           (current-line-sep)
           (hbl-append (* 2 gap-size)
                       (sub (w (encolors (lam x_1 e_1) scope1 scope2))
                            (w (encolors x_2 scope1))
                            (w (encolors e_2 scope3 scope4)))
                       (V sos-rhs? (t "="))
                       (V sos-rhs?
                          (sub #:<- <~
                               (w (encolors (lam x_1 e_1) scope1 scope2 scope6))
                               (w (encolors x_2 scope1 scope6))
                               (w (encolors e_2 scope3 scope4)))))
           (V sos-rhs?
              (hbl-append (t "where ") (encolors (ghost (tt " ")) scope6) (t " is fresh"))))))
      1.2))))

(define (reduction-slides)
  (make-reduction-slide)
  (make-reduction-slide #:rs-rhs? #t)
  (make-reduction-slide #:sos-lhs? #t)
  (make-reduction-slide #:sos-rhs? #t))

(module+ main
  (reduction-slides))
