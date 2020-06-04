#lang slideshow
(require racket/draw
         racket/class)

(provide tts
         over-cellophane
         shifted
         blend-color
         scale-color
         lighter
         darker
         background
         evenize-width
         unsmoothed
         extract-scale)

(define (tts . l)
  (apply vl-append
         (current-line-sep)
         (map tt l)))

(define (over-cellophane p a)
  (cond
    [(= a 1) p]
    [(= a 0) (ghost p)]
    [else
     (cc-superimpose
      p
      (cellophane (colorize (filled-rectangle (pict-width p) (pict-height p)) "white")
                  (- 1 a)))]))

(define (shifted f dx dy)
  (lambda (p q)
    (let-values ([(x y) (f p q)])
      (values (+ x dx) (+ y dy)))))

(define (blend-color c s)
  (cond
   [(string? c) (blend-color (make-object color% c) s)]
   [(string? s) (blend-color c (make-object color% s))]
   [else
    (define (blend a b) (quotient (+ a b) 2))
    (make-color (blend (send c red) (send s red))
                (blend (send c green) (send s green))
                (blend (send c blue) (send s blue)))]))

(define (scale-color c s)
  (cond
   [(string? c) (scale-color (make-object color% c) s)]
   [(list? c) (make-color (car c) (cadr c) (caddr c))]
   [else (make-color (min 255 (inexact->exact (floor (* s (send c red)))))
                     (min 255 (inexact->exact (floor (* s (send c green)))))
                     (min 255 (inexact->exact (floor (* s (send c blue))))))]))

(define (darker c) (scale-color c #e0.9))
(define (lighter c) (scale-color c #e1.1))

(define (background p color)
  (define bg
    (colorize (filled-rounded-rectangle (+ 4 (pict-width p))
                                        (+ 4 (pict-height p))
                                        8)
              color))
  (refocus (cc-superimpose bg p) p))

(define (evenize-width p)
  (let ([w (pict-width p)])
    ;; Force even size:
    (inset p 0 0 (+ (- (ceiling w) w)
                    (modulo (ceiling w) 2)) 0)))

(define (unsmoothed p)
  (dc (lambda (dc x y)
        (define s (send dc get-smoothing))
        (send dc set-smoothing 'unsmoothed)
        (draw-pict p dc x y)
        (send dc set-smoothing s))
      (pict-width p) (pict-height p)))

(define (extract-scale p q)
  (define-values (lx ty) (lt-find p q))
  (define-values (rx by) (rb-find p q))
  (/ (- rx lx) (pict-width q)))

