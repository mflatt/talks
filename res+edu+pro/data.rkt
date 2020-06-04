#lang slideshow
(require slideshow/play
         racket/draw
         racket/class
         "util.rkt")

(provide data)

(define (filled-polygon w h ls)
  (define no-pen (make-pen #:style 'transparent))
  (dc (lambda (dc x y)
        (define old-pen (send dc get-pen))
        (define old-brush (send dc get-brush))
        (send dc set-pen no-pen)
        (send dc set-brush (make-brush #:color (send old-pen get-color)))
        (send dc draw-polygon ls x y)
        (send dc set-pen old-pen)
        (send dc set-brush old-brush))
      w h))

(define (filled-trangle w h)
  (filled-polygon w h
                  (list (cons (/ w 2) 0)
                        (cons 0 h)
                        (cons w h))))

(define (filled-processor w dw h dh)
  (filled-polygon (+ w dw) (+ h (* 2 dh))
                  (list (cons 0 0)
                        (cons 0 dh)
                        (cons (* 0.5 w) (+ h dh))
                        (cons 0 (+ h dh))
                        (cons 0 (+ h (* 2 dh)))
                        (cons (+ dw w) (+ h (* 2 dh)))
                        (cons (+ dw w) (+ h dh))
                        (cons (+ dw (* 0.5 w)) (+ h dh))
                        (cons (+ dw w) dh)
                        (cons (+ dw w) 0))))

(define (data #:swap-n [swap-n 0]
              #:swap2-n [swap2-n 0])
 (let* ([w 100]
        [t (filled-trangle 100 100)]
        [c (lambda (s) "gold")]
        [top (colorize t (scale-color (c "red") 0.5))]
        [left (scale (colorize t (scale-color (c "blue") 0.5)) 0.8)]
        [right (scale (colorize t (scale-color (c "green") 0.5)) 0.6)]
        [bottom-left (scale (colorize t (scale-color (c "white") 0.5)) 0.3)]
        [bottom-right (scale (colorize t (scale-color (c "gold") 0.5)) 0.3)]
        [g-left (ghost left)]
        [g2-left (ghost left)]
        [g-right (ghost right)]
        [g-bottom-left (ghost bottom-left)]
        [g-bottom-right (ghost bottom-right)]
        [g2-bottom-right (ghost bottom-right)]
        [p (vc-append
            (* w -0.1)
            (vc-append (* w 0.2)
                       top
                       (ht-append (* w 0.2)
                                  (inset g-left
                                         0 0 (* w 0.1) 0)
                                  (inset g-right
                                         (* w 0.2) 0 (* w 0.1) 0)))
            (ct-superimpose
             (inset g-bottom-left
                    (* w 0.5) 0 0 0)
             (inset g-bottom-right
                    (* w 1.9) 0 0 0)))]
        [connect (lambda (p from from-find to dir)
                   (pin-line p
                             from (shifted from-find (* 1.5 dir) 0)
                             to (shifted ct-find  (* 1.5 dir) 0)
                             #:color "gray"
                             #:line-width 3))]
        [p (connect p top lb-find g-left 1)]
        [p (connect p top rb-find g-right -1)]
        [pin (lambda (p x g-x [g2-x g-x] [n 0])
               (define-values (x-x x-y) (ct-find p g-x))
               (define-values (x2-x x2-y) (ct-find p g2-x))
               (pin-over p
                         (+ (* (- 1 n) x-x) (* n x2-x))
                         (+ (* (- 1 n) x-y) (* n x2-y))
                         (inset x (* -1/2 (pict-width x)) 0)))]
        [pin-relative (lambda (p x wrt g-x g-wrt)
                        (define-values (g-x-x g-x-y) (lt-find p g-x))
                        (define-values (g-wrt-x g-wrt-y) (lt-find p g-wrt))
                        (define-values (wrt-x wrt-y) (lt-find p wrt))
                        (pin-over p
                                  (+ wrt-x (- g-x-x g-wrt-x))
                                  (+ wrt-y (- g-x-y g-wrt-y))
                                  x))]
        [p (pin p right g-right g-left swap-n)]
        [p (pin p g2-left g-left g-right swap-n)]
        [p (pin-relative p bottom-left right g-bottom-left g-right)]
        [p (pin-relative p g2-bottom-right right g-bottom-right g-right)]
        [p (pin p left g2-left g2-bottom-right swap2-n)]
        [p (pin p bottom-right g2-bottom-right g2-left swap2-n)]
        [p (connect p right lb-find bottom-left 1)]
        [p (connect p right rb-find g2-bottom-right -1)])
   p))

(module+ main
  (play-n
   #:skip-last? #t
   (lambda (n) (data #:swap-n (fast-middle n))))
  (play-n
   #:skip-first? #t
   (lambda (n) (data #:swap-n 1 #:swap2-n (fast-middle n)))))
