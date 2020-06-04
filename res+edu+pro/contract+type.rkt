#lang slideshow
(require racket/runtime-path
         slideshow/code
         pict/flash
         "history.rkt"
         "in-file.rkt"
         "util.rkt"
         "skull.rkt"
         "balloon.rkt"
         "paper.rkt")

(provide contract+type-slides

         contract-code
         contract-icon
         type-code
         type-icon)

(define poison (lift-above-baseline (scale (skull) 0.25) -3))

(define crash-color "yellow")

(define-runtime-path spidey-ppm "spidey.ppm")

(define sba-arrow-color "blue")
(define sba-arrow-size (/ gap-size 2))
(define sba-arrow-line-width 4)

(define-syntax-rule (code/b&w e)
  (parameterize ([code-colorize-enabled #f])
    (code e)))

(define (placeholder s)
  (ghost (tt (string-append s ".rkt"))))

(define (contract-code)
  (code (number? . -> . number?)))

(define (type-code)
  (code (Number -> Number)))

(define (contract-icon)
  (paper #:contract? #t))

(define (type-icon)
  (let ([p (inset (scale (rt "Γ ⊢ τ") 1.5) 10 0)])
    (background (colorize p "white") "cadetblue")))

(define (poison-slide #:crash? [crash? #f]
                      #:check? [check? #f]
                      #:callback? [callback? #f]
                      #:contract? [contract? #f]
                      #:type? [type? #f]
                      #:sba? [sba? #f]
                      #:modular? [modular? #f]
                      #:ca [use-ca #f])
  (define W 450)
  (define H 300)
  (define M (/ gap-size 2))
  (define (splash p color)
    (refocus (cc-superimpose
              (colorize
               (filled-flash (* 1.3 (pict-width p))
                             (* 1.3 (pict-height p)))
               color)
              p)
             p))
  (define (crash p)
    (if crash?
        (splash p crash-color)
        p))
  (define (ill-typed p)
    (cond
      [(or type? (and sba? modular?))
       (splash p "pink")]
      [contract?
       (splash p crash-color)]
      [else p]))
  (define (callback-fail p)
    (cond
      [(and sba? (not modular?))
       (splash p "pink")]
      [else p]))
  (define check-code
    (if (and check? (not callback?))
        (code (unless (number? x)
                (error ....)))
        (if (or contract? type?)
            (code code:blank)
            (code code:blank
                  code:blank))))
  (define (around-code p)
    (if (or contract? type?)
        (code code:blank
              #,p)
        p))
  (define x-arg (code x))
  (define x-use (code x))
  (define callback-body (callback-fail (and callback? (crash (code (+ #,x-use 1))))))
  (define help-arg (if callback? (code _proc) (code _x)))
  (define helper-module
    (in-file
     #:name (placeholder "helper")
     #:content-only? (and sba? (not modular?))
     (lc-superimpose
      (inset
       (around-code
        (code (define (help #,help-arg)
                #,(let* ([body-expr (if callback?
                                        (ill-typed (code (_proc #,poison)))
                                        (crash (code (+ _x 1))))]
                         [body (code
                                .... #,body-expr ....)])
                    (if check-code
                        (code #,check-code
                              #,body)
                        body)))))
       M)
      (blank W H))))
  (define ex
    (if modular?
        helper-module
        (ht-append
         (in-file
          #:name (placeholder "client")
          #:content-only? (and sba? (not modular?))
          (cc-superimpose
           (inset
            (code
             (help #,(if callback?
                         (code (λ (#,x-arg)
                                 #,callback-body))
                         poison))
             #,(if callback?
                   (code code:blank)
                   (code code:blank
                         code:blank))
             code:blank)
            M)
           (blank W H)))
         (hb-append
          (let ([sep (filled-rectangle (* 3 gap-size) (+ H (current-font-size)))])
            (if sba?
                (colorize sep "darkgray")
                (ghost sep)))
          helper-module))))
  (slide
   #:title (cond
             [use-ca use-ca]
             [sba? (ca 2006 "Modular Set-Based Analysis" #:who "Meunier")]
             [contract? (ca 2002 "Contracts" #:who "Findler")]
             [type? (ca 2008 "Typed Racket" #:who "Tobin-Hochstadt")]
             [else (ca 2002 "Experience with Modules")])
   #:name (cond
            [sba? "Modular Set-Based Analysis"]
            [contract? "Contracts"]
            [type? "Typed Racket"]
            [else "Experience with Modules"])
   (leftward (let* ([ex (if (or contract? type?)
                            (pin-balloon ex
                                         (if contract?
                                             (let ([p (contract-code)])
                                               (refocus (vl-append
                                                         gap-size
                                                         (contract-icon)
                                                         p)
                                                        p))
                                             (let ([p (type-code)])
                                               (refocus (vl-append
                                                         gap-size
                                                         (type-icon)
                                                         p)
                                                        p)))
                                         help-arg ct-find
                                         #:spike 's
                                         #:sprout 0.70)
                            ex)]
                    [ex (if (and sba? (not modular?))
                            (let* ([ex (pin-arrow-line
                                        sba-arrow-size
                                        ex
                                        poison lc-find
                                        x-arg rc-find
                                        #:color sba-arrow-color
                                        #:line-width sba-arrow-line-width)]
                                   [ex (pin-arrow-line
                                        sba-arrow-size
                                        ex
                                        x-arg cc-find
                                        x-use (shifted cc-find 0 -6)
                                        #:color sba-arrow-color
                                        #:line-width sba-arrow-line-width)]
                                   [ex (pin-balloon ex
                                                    poison
                                                    x-use (shifted ct-find 0 3)
                                                    #:spike 'n)])
                              ex)
                            ex)])
               ex))))

(define (soft-typing-slide #:when year #:who who
                           #:recursive-types? [recursive-types? #f])
  (define (math s)
    (cond
      [(regexp-match-positions #rx"[a-zA-Z]" s)
       => (lambda (m)
            (hbl-append
             (math (substring s 0 (caar m)))
             (text (substring s (caar m) (cdar m)) `(italic . roman) (current-font-size))
             (math (substring s (cdar m)))))]
      [(regexp-match-positions #rx"_[0-9]" s)
       => (lambda (m)
            (hbl-append
             (math (substring s 0 (caar m)))
             (text (substring s (add1 (caar m)) (cdar m)) `(subscript . roman) (current-font-size))
             (math (substring s (cdar m)))))]
      [else (rt s)]))
  (slide
   #:title (ca year "Soft Typing" #:who who)
   #:name "Soft Typing"
   (leftward
    (hc-append
     (* 3 gap-size)
     (scale (code (help (λ (x)
                          (+ x 1))))
            1)
     (colorize (arrow gap-size 0) "forestgreen")
     (let ([basic (let ([p (hbl-append gap-size
                                       (math "Γ ⊢ e_1 : τ_1 → τ_2")
                                       (math "Γ ⊢ e_2 : τ_1"))])
                    (vc-append (vc-append (current-line-sep)
                                          p
                                          (hline (+ (pict-width p) gap-size) 0))
                               (math "Γ ⊢ e_1 e_2 : τ_2")))])
       basic
       (if recursive-types?
           (refocus (vc-append (* 2 gap-size)
                               basic
                               (let ([p (math "⊢ T_1 ⟷ T_2[A ← μA.T_2]")])
                                 (vc-append (vc-append (current-line-sep)
                                                       p
                                                       (hline (+ (pict-width p) gap-size) 0))
                                            (math "⊢ T_1 ⟷ μA.T_2"))))
                    basic)
           basic))))))

(define (contract+type-slides)
  (as-history
   #:res 0
   #:edu 0
   (poison-slide)
   (poison-slide #:crash? #t)
   (poison-slide #:check? #t)
   (poison-slide #:callback? #t)
   (poison-slide #:callback? #t #:crash? #t))
  
  (as-history
   #:res 2
   #:edu 0
   (poison-slide #:callback? #t #:contract? #t)
   (poison-slide #:callback? #t #:type? #t))

  (define ca-spidey
    (ca 1997 "Set-Based Analysis" #:who "Flanagan"))
  
  (as-history
   #:res 2
   #:edu 0
   #:prod 0
   (soft-typing-slide #:when 1992 #:who "Fagan")
   (soft-typing-slide #:when 1994 #:who "Wright" #:recursive-types? #t)
   (poison-slide #:callback? #t #:sba? #t #:ca ca-spidey))

  (as-history
   #:res 2
   #:prod 0
   (slide
    #:title ca-spidey
    #:name "Set-Based Analysis"
    (leftward (scale (bitmap spidey-ppm) 1.2))))

  (as-history
   #:res 2
   #:edu 0
   #:prod 0
   (poison-slide #:callback? #t #:contract? #t #:sba? #t #:modular? #t))

  (as-history
   #:res 2
   #:edu 0
   (poison-slide #:callback? #t #:type? #t #:modular? #t))

  (void))

(module+ main
  (contract+type-slides))
