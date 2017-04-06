#lang slideshow
(require slideshow/code
         slideshow/balloon
         slideshow/play)

(provide phases-slides)

(define (phases-slides)
  (parameterize ([current-keyword-list
                  (list* "for-label"
                         "for-syntax"
                         "scheme"
                         "lexer"
                         (current-keyword-list))])
    (play-n
     #:name "Phases"
     (lambda (rt-n balloon-n rtex-n rtexdone-n ct-n ctex-n ctexdone-n dt-n dtex-n)
       (define-values (rt1-n rt2-n) (split-phase rt-n))
       (define-values (ct1-n ct2-n) (split-phase ct-n))
       (define-values (ctex1-n ctex2-n) (split-phase ctex-n))
       (define-values (dt1-n dt2-n) (split-phase dt-n))
       (define-values (dtex1-n dtex2-n) (split-phase dtex-n))
       (define (steps pre rt ct dt)
         (fade-pict rtex-n
                    pre
                    (fade-pict ctex1-n
                               (cellophane rt (- 1 rtexdone-n))
                               (fade-pict dtex-n
                                          (cellophane
                                           (cellophane ct ctex2-n)
                                           (- 1 ctexdone-n))
                                          (cellophane dt dtex2-n)))))
       (define (boxed p #:n [n 1]) 
         (ct-superimpose (blank client-w (* 2 n (current-font-size))) p))
       (define modname (let ([m (code parser-tools/lexer)])
                         (refocus
                          (pin-over
                           (launder
                            (cellophane
                             (pin-balloon (wrap-balloon (code
                                                         #,(colorize (tt "#lang") (current-base-color)) scheme
                                                         (provide lexer)
                                                         ....)
                                                        's 0 gap-size)
                                          (ghost m)
                                          m
                                          ct-find)
                             (* balloon-n (- 1 rtexdone-n))))
                           0 0
                           m)
                          m)))
       (vc-append
        gap-size
        (boxed
         (code (require #,(fade-around-pict
                           ct-n
                           (fade-pict rt1-n
                                      (code ....)
                                      (cellophane modname rt2-n))
                           (lambda (g)
                             (let ([g1 (launder g)]
                                   [g2 (launder g)])
                               (slide-pict
                                (fade-pict
                                 dt1-n
                                 (cellophane (code (for-syntax #,g1)) ct2-n)
                                 (cellophane (code (for-label #,g2)) dt2-n))
                                g
                                g1 g2 dt-n)))))))
        (boxed
         (let ([tm (lambda (s) (para (bt s) "time:"))])
           (steps (tt " ")
                  (tm "Run")
                  (tm "Expand")
                  (tm "Document-reading"))))
        (boxed
         #:n 2
         (cellophane
          (fade-around-pict
           (* ctex1-n (- 1 ctexdone-n))
           (fade-around-pict
            dtex1-n
            (code (lexer ....))
            (lambda (g)
              (cellophane
               (code (scheme #,g))
               dtex2-n)))
           (lambda (g)
             (cellophane
              (code (define-syntax my-macro
                      .... #,g ....))
              (* ctex2-n (- 1 ctexdone-n)))))
          rtex-n)))))))
