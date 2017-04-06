#lang slideshow
(require racket/class
         racket/draw)

(provide tree-pict
         branch
         curly
         icon)

(struct branch (dir len forks))
(struct curly (size left?))
(struct icon (pict draw? next))

(define (tree-path t icons)
  (define (tree p b x y r)
    (cond
     [(branch? b)
      (define straight (make-polar (branch-len b) r))
      (define nr (+ r (branch-dir b)))
      (define next (make-polar (branch-len b) nr))
      (define sx (real-part straight))
      (define sy (- (imag-part straight)))
      (define nx (real-part next))
      (define ny (- (imag-part next)))
      
      (send p curve-to
            (+ x (/ sx 2)) (+ y (/ sy 2))
            (+ x (/ nx 2)) (+ y (/ ny 2))
            (+ x nx) (+ y ny))
      
      (define forks (branch-forks b))
      (cond
       [(not (list? forks))
        (tree p forks (+ x nx) (+ y ny) nr)]
       [(and (pair? forks)
             (null? (cdr forks)))
        (tree p (car forks) (+ x nx) (+ y ny) nr)]
       [else
        (cons p
              (apply
               append
               (for/list ([b (in-list forks)])
                 (define p (new dc-path%))
                 (send p move-to (+ x nx) (+ y ny))
                 (tree p b (+ x nx) (+ y ny) nr))))])]
     [(icon? b)
      (define bp (icon-pict b))
      (define straight (make-polar 1 r))
      (define rsx (real-part straight))
      (define rsy (- (imag-part straight)))
      (define s (cond
                 [(zero? rsx)
                  (/ (pict-height bp) (abs rsy))]
                 [(zero? rsy)
                  (/ (pict-width bp) (abs rsx))]
                 [else
                  (min (/ (pict-width bp) (abs rsx))
                       (/ (pict-height bp) (abs rsy)))]))
      (define sx (* s rsx))
      (define sy (* s rsy))
      (cond
       [(icon-draw? b)
        (set-box! icons (cons (list bp
                                    (- (+ x (/ sx 2))
                                       (/ (pict-width bp) 2))
                                    (- (+ y (/ sy 2))
                                       (/ (pict-height bp) 2)))
                              (unbox icons)))
        (cons p
              (let ([p (new dc-path%)])
                (send p move-to (+ x sx) (+ y sy))
                (tree p (icon-next b) (+ x sx) (+ y sy) r)))]
       [else
        (send p line-to (+ x sx) (+ y sy))
        (tree p (icon-next b) (+ x sx) (+ y sy) r)])]
     [(curly? b)
      (define c-len (curly-size b))
      (define c (make-polar c-len (+ r (* (if (curly-left? b) 1/2 -1/2) pi))))
      (define c0 (make-polar c-len r))
      (define cx0 (real-part c0))
      (define cy0 (- (imag-part c0)))
      (define cx (real-part c))
      (define cy (- (imag-part c)))
      (send p curve-to
            (+ x cx0) (+ y cy0)
            (+ x cx cx0) (+ y cy cy0)
            (+ x cx) (+ y cy))
      (send p curve-to
            (+ x cx (* -0.5 cx0)) (+ y cy (* -0.5 cy0))
            (+ x (/ (- cx cx0) 2)) (+ y (/ (- cy cy0) 2))
            (+ x (/ cx 2)) (+ y (/ cy 2)))
            (list p)]
     [else
      (error 'tree "bad piece: ~s" b)]))
  (define p (new dc-path%))
  (send p move-to 0 0)
  (tree p t 0 0 (* pi 1/2)))

(define green-pen (make-pen #:color "forestgreen" #:width 2))
(define no-brush (make-brush #:style 'transparent))

(define (tree-pict t #:pip? [pip? #f])
  (define icons (box null))
  (let ([ps (tree-path t icons)])
    (define-values (l t r b)
      (if pip?
          (values 0 0 0 0)
          (let-values ([(x y w h) (send (car ps) get-bounding-box)])
            (for/fold ([l x] [t y] [r (+ x w)] [b (+ y h)]) ([p (in-list (cdr ps))])
              (define-values (x y w h) (send p get-bounding-box))
              (values (min l x)
                      (min t y)
                      (max r (+ x w))
                      (max b (+ y h)))))))
    (define base-p
      (dc (lambda (dc x y)
            (define p (send dc get-pen))
            (define b (send dc get-brush))
            (send dc set-pen green-pen)
            (send dc set-brush no-brush)
            (for ([p (in-list ps)])
              (send dc draw-path p (- x l) (- y t)))
            (send dc set-pen p)
            (send dc set-brush b))
          (- r l) (- b t)))
    (for/fold ([p base-p]) ([i (in-list (unbox icons))])
      (pin-over p
                (- (cadr i) l) (- (caddr i) t)
                (car i)))))

(module+ main
  (slide
   (tree-pict (branch (* -1/3 pi) 30 (branch (* -1/3 pi) 30 (icon (rectangle 20 10) #f (branch (* -1/3 pi) 30 null)))))
   (tree-pict (branch (* -1/3 pi) 50 (branch (* -1/3 pi) 50 (branch (* -1/3 pi) 50 null))))
   (tree-pict (branch 0 0 (list
                           (branch (* -1/2 pi) 100
                                   (curly 20 #t))
                           (branch (* 1/2 pi) 100
                                   (curly 20 #f)))))
   (tree-pict
    (branch 0.0
            30
            (list
             (branch (* 3/4 pi)
                     30
                     (branch 0 30 (curly 15 #t)))
             (branch 0 40 (curly 20 #f)))))))

;; ----------------------------------------

;; Cool-looking "tree"s, but doesn't really fit the theme:

(define (vine p from to odd?)
  (define-values (fx0 fy) (ct-find p from))
  (define-values (tx ty) (cb-find p to))
  (define dist (abs (- tx fx0)))
  (define fx (+ fx0 (- (remainder (round dist) 20) 10)))
  (define s1 (* (- fy ty) (/ 2 (+ 3 (quotient (round dist) 100)))))
  (define drift (/ (if odd? -1/10 1/10) (add1 (quotient (round dist) 100))))
  (pin-under p
             fx fy
             (tree-pict #:pip? #t
                        (branch (* (* 1/300 (- fx fx0)) pi)
                                s1
                                (branch (if (tx . < . fx)
                                            (* 1/2 pi)
                                            (* -1/2 pi))
                                        (* 1/3 dist)
                                        (branch (* drift pi)
                                                (* 1/3 dist)
                                                (list
                                                 (curly 20 odd?)
                                                 (branch (* (- drift) pi)
                                                         (* 1/3 dist)
                                                         (branch (+ (if (tx . < . fx)
                                                                        (* -1/2 pi)
                                                                        (* 1/2 pi))
                                                                    (* 0.1 (- (remainder (round dist) 4) 2)))
                                                                 (- fy ty s1)
                                                                 null)))))))))

(define (vines p from tos start-i)
  (for/fold ([p p]) ([to (in-list tos)]
                     [i (in-naturals (or start-i 0))])
    (vine p from to (or (not start-i) (odd? i)))))

(define (viness p froms tos)
  (for/fold ([p p]) ([from (in-list froms)]
                     [i (in-naturals)])
    (vines p from tos i)))
