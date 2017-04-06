#lang racket/base
(require slideshow
         racket/draw
         racket/class)

(provide blueprint)

(define (extract-face l)
  (cond
   [(and (pair? l) (string? (car l))) (car l)]
   [(pair? l) (extract-face (cdr l))]
   [(string? l) l]
   [else #f]))

(define (extract-family l)
  (cond
   [(pair? l) (extract-family (cdr l))]
   [(symbol? l) l]
   [else #f]))

(define outline-lambda
  (let ([tp (t "λ")]
        [font (make-font 
               #:face (extract-face (current-main-font))
               #:family (extract-family (current-main-font))
               #:size (current-font-size)
               #:size-in-pixels? #t)])
    (define w (pict-width tp))
    (define h (pict-height tp))
    (define p (new dc-path%))
    (send p text-outline font "λ" 0 0)
    (dc (lambda (dc x y)
          (define pen (send dc get-pen))
          (define brush (send dc get-brush))
          (send dc set-pen (make-pen #:style 'long-dash
                                     #:width (send pen get-width)
                                     #:color (send pen get-color)))
          (send dc set-brush (make-brush #:style 'transparent))
          (send dc draw-path p x y)
          (send dc set-brush brush)
          (send dc set-pen pen))
        w h)))
          
(define blueprint
  (let* ([p (inset (scale outline-lambda 5) (* 2 gap-size) 0)]
         [r (colorize (filled-rectangle (pict-width p) (pict-height p)) "blue")])
    (refocus (cc-superimpose
              r
              (colorize (inset (vline 0 (pict-height p)) 0 0 (* 3 gap-size) 0) "lightblue")
              (colorize (inset (hline (pict-width p) 0) 0 0 0 (* 3 gap-size)) "lightblue")
              (colorize p "white"))
             r)))

