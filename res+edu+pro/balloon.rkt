#lang racket/base
(require pict
         racket/draw
         racket/class
         racket/math)

(provide pin-balloon)

(define current-balloon-color (make-parameter (make-color 255 255 170)))

(define (pin-balloon p
                     content
                     wrt-p wrt-p-find
                     #:spike spike
                     #:margin [margin 10]
                     #:corner-radius [corner-radius 10]
                     #:dx [dx (case spike
                                [(sw w nw) -20]
                                [(se e ne) 20]
                                [else 0])]
                     #:dy [dy (case spike
                                [(sw s se) 20]
                                [(nw n ne) -20]
                                [else 0])]
                     #:sprout [sprout 0.5] ; applies to n, s, e, w
                     #:thought? [thought? #f]
                     #:color [color (current-balloon-color)]
                     #:line-color [line-color #f]) 
  (define w (+ (pict-width content) (* 2 margin)))
  (define h (+ (pict-height content) (* 2 margin)))
  (define rx corner-radius)
  (define ry corner-radius)
  (define-values (x y) (wrt-p-find p wrt-p))
  (define sx (case spike
               [(s n) (* w sprout)]
               [else (/ w 2)]))
  (define sy (case spike
               [(e w) (* h sprout)]
               [else (/ h 2)]))
  (define bx
    (- (case spike
         [(w nw sw) x]
         [(s n) (- x sx)]
         [else (- x w)])
       dx))
  (define by
    (- (case spike
         [(n nw ne) (+ y sy)]
         [(e w) (- y sy)]
         [else (- y h)])
       dy))
  (define pen (if line-color
                  (make-pen #:color line-color)
                  (make-pen #:style 'transparent)))
  (define brush (if color
                    (make-brush #:color color)
                    (make-brush #:style 'transparent)))
  (define balloon-path
    (cond
      [(not thought?)
       (let ([p (new dc-path%)])
         (send p move-to rx 0)
         (case spike
           [(n) (send p line-to (- sx rx) 0)
                (send p line-to sx dy)
                (send p line-to (+ sx rx) 0)
                (send p line-to (- w rx) 0)]
           [else (send p line-to (- w rx) 0)])
         (case spike
           [(ne) (send p line-to (+ w dx) dy)
                 (send p line-to w ry)]
           [else (send p arc (- w (* 2 rx)) 0 (* 2 rx) (* 2 ry) (* 1/2 pi) 0 #f)])
         (send p line-to w (- h ry))
         (case spike
           [(se) (send p line-to (+ w dx) (+ h dy))
                 (send p line-to (- w rx) )]
           [else (send p arc (- w (* 2 rx)) (- h (* 2 ry)) (* 2 rx) (* 2 ry) 0 (* -1/2 pi) #f)])
         (case spike
           [(s) (send p line-to (+ sx rx) h)
                (send p line-to (+ sx dx) (+ h dy))
                (send p line-to (- sx rx) h)
                (send p line-to rx h)]
           [else (send p line-to rx h)])
         (case spike
           [(sw) (send p line-to dx (+ h dy))
                 (send p line-to 0 (- h ry))]
           [else (send p arc 0 (- h (* 2 ry)) (* 2 rx) (* 2 ry) (* -1/2 pi) (- pi) #f)])
         (send p line-to 0 ry)
         (case spike
           [(nw) (send p line-to dx dy)]
           [else (send p arc 0 0 (* 2 rx) (* 2 ry) (- pi) (* 1/2 pi) #f)])
         (send p close)
         p)]
      [else
       (let ([p (new dc-path%)])
         (send p move-to (* 2/3 rx) (* 1/3 h))
         (send p curve-to
               0 0
               (* 1/8 w) 0
               (* 1/4 w) (* 2/3 ry))
         (send p curve-to
               (* 3/10 w) (* -1/2 ry)
               (* 3/8 w) (* -1/2 ry)
               (* 11/20 w) (* 2/3 ry))
         (send p curve-to
               (* 5/8 w) (* -1/2 ry)
               (* 11/16 w) (* -1/2 ry)
               (* 3/4 w) (* 2/3 ry))
         (send p curve-to
               (* 8/10 w) 0
               w 0
               (- w (* 2/3 rx)) (* 1/4 h))
         (send p curve-to
               (+ w (* 1/2 rx)) (* 3/8 h)
               (+ w (* 1/2 rx)) (* 1/2 h)
               (- w (* 2/3 rx)) (* 5/8 h))
         (send p curve-to
               (+ w (* 1/2 rx)) (* 2/3 h)
               (+ w (* 1/2 rx)) (* 7/10 h)
               (- w (* 2/3 rx)) (* 4/5 h))
         (send p curve-to
               w (* 9/10 h)
               w (+ h (* 1/2 ry))
               (* 3/4 w) (- h (* 2/3 ry)))
         (send p curve-to
               (* 4/10 w) (+ h (* 1/2 ry))
               (* 1/2 w) (+ h (* 1/2 ry))
               (* 1/4 w) (- h (* 2/3 ry)))
         (send p curve-to
               (* 1/8 w) (+ h (* 1/2 ry))
               0 (+ h (* 1/2 ry))
               (* 2/3 rx) (* 5/6 h))
         (send p curve-to
               (* -1/2 rx) (* 3/4 h)
               (* -1/2 rx) (* 3/4 h)
               (* 2/3 rx) (* 1/2 h))
         (send p curve-to
               (* -1/2 rx) (* 7/16 h)
               (* -1/2 rx) (* 7/16 h)
               (* 2/3 rx) (* 1/3 h))
         (send p close)
         (case spike
           [(s)
            (define s (/ dy 3))
            (send p ellipse (/ (- w s) 2) (+ h (/ s 3)) s s)
            (send p ellipse (/ (- w (/ s 2)) 2) (+ h (* s 7/4)) (/ s 2) (/ s 2))]
           [else (error "not ready")])
         p)]))
  (define balloon
    (dc (lambda (dc x y)
          (define p (send dc get-pen))
          (define b (send dc get-brush))
          (send dc set-pen pen)
          (send dc set-brush brush)
          (send dc draw-path balloon-path x y)
          (send dc set-pen p)
          (send dc set-brush b))
        w h))
  (pin-over (pin-over p
                      bx by
                      balloon)
            (+ bx margin) (+ by margin)
            content))

(module+ main
  (require slideshow)
  (define square (filled-rectangle 100 100))
  (slide
   (pin-balloon square
                (vc-append
                 (current-line-sep)
                 (t "To be or not to be —")
                 (t "that is the question"))
                square ct-find
                #:margin 32
                #:dy 50
                #:spike 's
                #:color "gainsboro"
                #:thought? #t)))
