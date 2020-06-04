#lang slideshow
(require slideshow/code
         racket/draw
         racket/class)

(provide chain1)

(define LINE-WIDTH 2)
(define link-arrow-size 16)
(define SMALL-LINK-LEN 30)

(define hole (colorize (parameterize ([code-colorize-enabled #f])
                         (code []))
                       "darkgray"))

(define (mk-down-link h)
  (linewidth LINE-WIDTH (inset (pip-arrow-line 0 h link-arrow-size) 0 0 0 h)))
(define short-down-link (mk-down-link link-arrow-size))
(define down-link (mk-down-link SMALL-LINK-LEN))
(define long-down-link (mk-down-link (* 2 SMALL-LINK-LEN)))

(define bubble-brush (make-brush #:color "lightgray"))
(define no-pen (make-pen #:style 'transparent))

(define (square-ellipse w h squareness)
  (let ([p (new dc-path%)]
        [c-ness (* 0.5 (- 1.0 squareness))]
        [z 0.4])
    (send p move-to 0 (* h c-ness))
    (send p curve-to 0 (* h c-ness z) (* w c-ness z) 0 (* w c-ness) 0)
    (send p line-to (* w (- 1.0 c-ness)) 0)
    (send p curve-to (* w (- 1.0 (* c-ness z))) 0 w (* h c-ness z) w (* h c-ness))
    (send p line-to w (* h (- 1.0 c-ness)))
    (send p curve-to w (* h (- 1.0 (* c-ness z))) (* w (- 1.0 (* c-ness z))) h (* w (- 1.0 c-ness)) h)
    (send p line-to (* w c-ness) h)
    (send p curve-to (* w c-ness z) h 0 (* h (- 1.0 (* c-ness z))) 0 (* h (- 1.0 c-ness)))
    (send p close)
    (dc (lambda (dc x y)
          (define old-p (send dc get-pen))
          (define old-b (send dc get-brush))
          (send dc set-pen no-pen)
          (send dc set-brush bubble-brush)
          (send dc draw-path p x y)
          (send dc set-brush old-b)
          (send dc set-pen old-p))
        w h)))

(define (interpolate n s e)
  (+ s (* (- e s) n)))

(define (bubble** c n)
  (let ([w (interpolate n 
                        (* 1.1 (pict-width c))
                        (+ (pict-width c) 8))]
        [h (interpolate n 
                        (* 2.5 (pict-height c))
                        (+ (pict-height c) 28))])
    (baselines (cc-superimpose (linewidth LINE-WIDTH (square-ellipse w h n))
                               c)
               c)))

(define (baselines p c)
  (let-values ([(x y) (lt-find p c)])
    (inset (refocus p c)
           x y 
           (- (pict-width p) x (pict-width c))
           (- (pict-height p) y (pict-height c)))))

(define (bubble c)
  (bubble** c 0))

(define (redex c)
  (bubble** c 1))

(define chain*
  (case-lambda
    [(link redex) redex]
    [(link bubble next . rest)
     (if (number? next)
         (use-last (vc-append 0
                              bubble
                              (cellophane
                               (vc-append 0
                                          link
                                          (apply chain* link rest))
                               next))
                   (let ([l (pict-last bubble)])
                     (if l
                         (if (pair? l)
                             (cons bubble l)
                             (list bubble l))
                         bubble)))
         (apply chain* link bubble 1.0 next rest))]
    [(link bubble . rest)
     (vc-append 0
                bubble
                link
                (apply chain* link rest))]))

(define (chain . rest)
  (apply chain* down-link rest))

(define (mv v . colors)
  (lambda (sub)
    (let ([v (text v `(italic . ,(current-code-font)) (current-font-size))])
      (if sub
          (let ([p (hbl-append v
                               (text (format "~a" sub) `(subscript italic . ,(current-code-font)) (current-font-size)))]
                [cm (assv sub colors)])
            (if cm
                (colorize p (cdr cm))
                p))
          v))))

(define v (mv "v"
              '(7 . "VioletRed")
              '(9 . "Peru")
              '(15 . "OliveDrab")))
(define -e (mv "e"))
(define (e n)
  (-e (case n
        [(3) 5]
        [(4) 6]
        [(5) 3]
        [(6) 4]
        [else n])))
(define x (mv "x"))

(define chain1-node1-code (code (#,(v 1) #,hole)))
(define chain1-node1 (bubble chain1-node1-code))

(define chain1-node2-code (code (#,hole #,(v 2))))
(define chain1-node2 (bubble chain1-node2-code))

(define chain1-before-body
  (code ((λ (x) x) #,(v 3))))

(define chain1
  (chain chain1-node1
         chain1-node2
         (redex chain1-before-body)))

(module+ main
  (slide chain1))
