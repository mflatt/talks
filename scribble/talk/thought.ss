#lang slideshow
(require scheme/class
         scheme/gui/base
         slideshow/play)

(provide thought)

(define (gray diss)
  (let ([N (inexact->exact (floor (+ 150 (* 105 diss))))])
    (make-object color% N N N)))

(define (make-thought w h #:dissolve [diss 0.0] #:fade [fade 1.0])
  (dc
   (let ([p (new dc-path%)]
         [s (- 1 (* diss 2/3))])
     (send p ellipse
           (- 0 (* diss 50)) (+ 0 (* 1/4 h))
           (* 1/2 w s) (* 1/2 h s))
     (send p ellipse
           (+ 0 (* 1/5 w)) (- 0 (* diss 50))
           (* 3/5 w s) (add1 (* 2/5 h s)))
     (send p ellipse
           (+ 0 (* 1/5 w)) (+ 0 (* 1/3 h) ( * diss 50))
           (* 3/5 w s) (* 2/3 h s))
     (send p ellipse
           (+ 0 (* 3/5 w) (* diss 50)) (+ 0 (* 1/4 h) (* diss -25))
           (* 2/5 w s) (* 1/3 h s))
     (send p ellipse
           (+ 0 (* 3/5 w) (* diss 40)) (+ 0 (* 1/2 h) (* diss 25))
           (* 2/5 w s) (* 1/3 h s))

     (lambda (dc x y)
       (let* ([ob (send dc get-brush)]
              [op (send dc get-pen)]
              [c (gray (max diss (- 1 fade)))])
         (send dc set-pen "white" 0 'transparent)
         (send dc set-brush c 'solid)
         
         (send dc draw-path p x y 'winding)

         (send dc set-brush ob)
         (send dc set-pen op))))
   w h))

(define (split-phase3 opt-n)
  (values (* 3 (min opt-n 0.33))
          (* 3 (- (max opt-n 0.33) 0.33))
          (min 1.0 (* 3 (- (max opt-n 0.66) 0.66)))))

(define (thought base from to grow-n gone-n
                 #:wrap-thought [wrap-thought values])
  (if (or (= 0.0 grow-n)
          (= 1.0 gone-n))
      base
      (let ([dir (let-values ([(x1 y1) (lt-find base from)]
                              [(x2 y2) (lt-find base to)])
                   (if (x1 . < . x2)
                       'right
                       'left))])
        (let-values ([(x0 y0) ((if (eq? dir 'left) lt-find rt-find) base from)]
                     [(x y) (lt-find base to)]
                     [(w) (pict-width to)]
                     [(h) (pict-height to)])
          (let ([x (- x (* 0.25 w))]
                [y (- y (* 0.25 h))]
                [w (* 1.5 w)]
                [h (* 1.5 h)]
                [delta (if (eq? dir 'left)
                           (- (+ x w) x0)
                           (- x x0))]
                [ydelta (- y0 (/ w 2) y)]
                [x-start (if (eq? dir 'left)
                             (+ (- x0 x) (/ w 4))
                             (- x0 x))]
                [xh (- y0 (+ y (/ h 2)))])
            (define-values (grow1x-n grow3-n) (split-phase grow-n))
            (define-values (grow0-n grow1-n grow2-n) (split-phase3 grow1x-n))
            (pin-under
             base
             x y
             (wrap-thought
              (let ([t (make-thought w h #:dissolve gone-n #:fade grow3-n)]
                    [sz (min (abs delta) h)])
                (pin-over
                 (pin-over
                  (pin-over
                   t
                   (+ x-start (* delta 2/3)) (+ (* 1/2 xh) (/ h 2))
                   (colorize (filled-ellipse (/ sz 3) (/ sz 3)) 
                             (gray (max gone-n (- 1 grow2-n)))))
                  (+ x-start (* delta 2/5)) (+ (* 3/4 xh) (/ h 2))
                  (colorize (filled-ellipse (/ sz 6) (/ sz 6)) 
                            (gray (max gone-n (- 1 grow1-n)))))
                 (+ x-start (* delta 1/10)) (+ xh (/ h 2))
                 (colorize (filled-ellipse (/ sz 8) (/ sz 8)) 
                           (gray (max gone-n (- 1 grow0-n)))))))))))))
