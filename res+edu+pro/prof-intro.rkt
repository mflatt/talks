#lang slideshow
(require slideshow/play
         "person.rkt"
         "venn.rkt"
         "bar.rkt")

(provide prof-intro-slides)

(define (prof-intro-slides)
  (define big-s 2)
  (define prof (professor))
  (define g-prof (ghost prof))
  (define g2-prof (ghost (scale prof big-s)))

  (define v+p (venn+prof g-prof))

  (define-values (gl gt) (lt-find v+p g-prof))
  (define-values (gr gb) (rb-find v+p g-prof))
  (define s (/ (- gr gl) (pict-width g-prof)))

  (play-n
   #:skip-last? #t
   (lambda (n)
     (define p (cc-superimpose (cellophane v+p (- 1 n))
                               g2-prof))
     (define pf (scale prof (+ s (* (fast-middle n) (- big-s s)))))
     (slide-pict p pf g-prof g2-prof (fast-middle n))))

  (define title (citizen-title g-prof))

  (define-values (tgl tgt) (lt-find title g-prof))
  (define-values (tgr tgb) (rb-find title g-prof))
  (define ts (/ (- tgr tgl) (pict-width g-prof)))

  (play-n
   #:skip-last? #t
   (lambda (n)
     (define pf (scale prof (+ ts (* (- 1 (fast-middle n)) (- big-s ts)))))
     (define p (cc-superimpose
                (ct-superimpose full-page
                                (cellophane title n))
                g2-prof))
     (slide-pict p pf g2-prof g-prof (fast-middle n)))))

(module+ main
  (prof-intro-slides))
