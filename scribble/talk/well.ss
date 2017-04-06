#lang slideshow
(require scheme/class
         scheme/gui/base
         "style.rkt")

(provide well well+sign well+sign* sign one-in-sign)

(define (mk-block w h dt dr db dl)
  (let ([p (new dc-path%)]
        [d 4]
        [tx 1/2]
        [rx 1/2]
        [bx 1/2]
        [lx 1/2])
    (send p move-to d 0)
    (send p curve-to (* w 1/3 tx) 0 (* w 2/3 tx) dt (* w tx) dt)
    (send p curve-to (* w (- 1 (* 2/3 (- 1 tx)))) dt (* w (- 1 (* 1/3 (- 1 tx)))) 0 (- w d) 0)
    (send p curve-to w 0 w 0 w d)
    (send p curve-to w (* h 1/3 rx) (- w dr) (* h 2/3 rx) (- w dr) (* h rx))
    (send p curve-to (- w dr) (* h (- 1 (* 2/3 (- 1 rx)))) w (* h (- 1 (* 1/3 (- 1 rx)))) w (- h d))
    (send p curve-to w h w h (- w d) h)
    (send p curve-to (* w (- 1 (* 1/3 (- 1 bx)))) h (* w (- 1 (* 2/3 (- 1 bx)))) (- h db) (* w bx) (- h db))
    (send p curve-to (* w 2/3 bx) (- h db) (* w 1/3 bx) h d h)
    (send p curve-to 0 h 0 h 0 (- h d))
    (send p curve-to 0 (* h (- 1 (* 1/3 (- 1 lx)))) dl (* h (- 1 (* 2/3 (- 1 lx)))) dl (* h lx))
    (send p curve-to dl (* h 2/3 lx) 0 (* h 1/3 lx) 0 d)
    (send p curve-to 0 0 0 0 d 0)
    (send p close)
    (inset
     (dc (lambda (dc x y)
           (let ([op (send dc get-pen)]
                 [ob (send dc get-brush)])
             (send dc set-pen "black" 2 'solid)
             (if 3-D?
                 (let ()
                   (send dc set-brush
                         (make-brush #:gradient (make-object linear-gradient%
                                                             x y
                                                             (+ x w) (+ y h)
                                                             (list
                                                              (list 0.0 (make-color 180 180 180))
                                                              (list 1.0 (make-color 100 100 100)))))))
                 (send dc set-brush "gray" 'solid))
             (send dc draw-path p x y)
             (send dc set-pen op)
             (send dc set-brush ob)))
         w h)
     1)))

(define (arch w)
  (let ([roof (new dc-path%)]
        [side (new dc-path%)]
        [w (* 1.2 w)])
    (send roof move-to 20 0)
    (send roof line-to 100 0)
    (send roof line-to 80 50)
    (send roof line-to 0 50)
    (send roof close)
    (send roof translate -10 0)
    (send roof scale (/ w 100) (/ w 100))

    (send side move-to 100 0)
    (send side line-to 80 50)
    (send side line-to 120 45)
    (send side close)
    (send side translate -10 0)
    (send side scale (/ w 100) (/ w 100))

    (dc (lambda (dc x y)
          (let ([op (send dc get-pen)]
                [ob (send dc get-brush)])
            (send dc set-pen "black" 2 'solid)
            (send dc set-brush "brown" 'solid)

            (send dc draw-rectangle 
                  (+ (* w 1/10) x) (+ (* w 3/10) y)
                  (* w 1/10) (* w 5/10))
            (send dc draw-rectangle 
                  (+ (* w 8/10) x) (+ (* w 3/10) y)
                  (* w 1/10) (* w 5/10))
            (send dc draw-path side x y)
            
            (when 3-D?
              (send dc set-brush
                    (make-brush #:gradient (make-object linear-gradient%
                                                        x y
                                                        (+ x (* 1/2 w)) (+ y (* 1/3 w))
                                                        (list (list 0 (make-object color% "chocolate"))
                                                              (list 1.0 (make-object color% "brown")))))))
            (send dc draw-path roof x y)

            (send dc set-pen op)
            (send dc set-brush ob)))
      w (* 0.8 w))))

(define (rt s)
  (text s `roman (current-font-size)))
(define (rit s)
  (text s `(italic . roman) (current-font-size)))

(define one-in-sign (scale (rt "1") 2))

(define sign
  (frame
   (inset (vc-append
           (scale (rit "Wishing Well") 0.5)
           (hline (* 5 gap-size) gap-size)
           one-in-sign
           (scale (rt "wish per princess") 0.5))
          (/ gap-size 3))))
  
(define well
  (let* ([base
          (vc-append
           (hc-append (mk-block 50 30 1 -1 1 -1)
                      (mk-block 30 30 0 -1 0 1)
                      (mk-block 35 30 1 0 1 0)
                      (mk-block 20 30 0 -1 1 -1))
           (hc-append (mk-block 40 30 -1 -1 1 -1)
                      (mk-block 25 30 0 -1 0 1)
                      (mk-block 30 30 1 0 1 0)
                      (mk-block 40 30 0 -1 1 -1))
           (hc-append (mk-block 15 30 -1 -1 1 -1)
                      (mk-block 55 30 1 0 1 0)
                      (mk-block 50 30 1 0 1 0)
                      (mk-block 15 30 0 -1 1 -1))  
           (hc-append (mk-block 40 30 -1 -1 1 -1)
                      (mk-block 40 30 0 -1 0 1)
                      (mk-block 30 30 1 0 1 0)
                      (mk-block 25 30 0 -1 1 -1)))]
         [base+ledge
          (vc-append
           base
           (hc-append (mk-block 30 10 0 -1 0 1)
                      (mk-block 35 10 1 0 1 0)
                      (mk-block 30 10 0 -1 1 -1)
                      (mk-block 80 10 1 -1 1 -1)))]
         [roof (arch (pict-width base))])
    (vc-append
     roof
     base+ledge)))

(define (well+sign* n)
  (vc-append
   (/ gap-size 2)
   (cellophane sign n)
   well))

(define well+sign (well+sign* 1.0))

