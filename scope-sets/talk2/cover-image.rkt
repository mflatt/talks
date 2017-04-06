#lang racket/base
(require racket/class
         racket/draw)

(provide cover-image-path
         cover-letters-rgb
         cover-base-rgb
         RR)

(define bm (make-bitmap 10 10))
(define dc (send bm make-dc))

(define font (make-font))

(define p (new dc-path%))

(define R 60)

(define top "RACKET DEPT. OF")
(for ([c (in-string top)])
  (define-values (w h d a) (send dc get-text-extent (string c)  font))
  (send p text-outline font (string c) (* w -1/2) (- R))
  (send p rotate 0.2))
(send p rotate (* -0.1 (add1 (string-length top))))

(define bottom "PUBLIC WORKS")
(send p rotate (* 0.1 (sub1 (string-length bottom))))
(for ([c (in-string bottom)])
  (define-values (w h d a) (send dc get-text-extent (string c)  font))
  (send p text-outline font (string c) (* w -1/2) (- R h))
  (send p rotate -0.2))
(send p rotate (* 0.1 (add1 (string-length bottom))))

(define lam-p
  (let ([p (new dc-path%)])
    (send p move-to 153 44)
    (send p line-to 161.5 60)
    (send p curve-to 202.5 49 230 42 245 61)
    (send p curve-to 280.06 105.41 287.5 141 296.5 186)
    (send p curve-to 301.12 209.08 299.11 223.38 293.96 244)
    (send p curve-to 281.34 294.54 259.18 331.61 233.5 375)
    (send p curve-to 198.21 434.63 164.68 505.6 125.5 564)
    (send p line-to 135 572)
    
    (send p line-to 135 572)
    (send p line-to 188.5 564)
    (send p curve-to 208.5 517 230.91 465.21 251 420)
    (send p curve-to 267 384 278.5 348 296.5 312)
    (send p curve-to 301.01 302.98 318 258 329 274)
    (send p curve-to 338.89 288.39 351 314 358 332)
    (send p curve-to 377.28 381.58 395.57 429.61 414 477)
    (send p curve-to 428 513 436.5 540 449.5 573)
    (send p line-to 465 580)
    (send p line-to 529 545)
    
    (send p line-to 153 44)
    (send p curve-to 192.21 30.69 233.21 14.23 275 20)
    (send p curve-to 328.6 27.4 350.23 103.08 364 151)
    (send p curve-to 378.75 202.32 400.5 244 418 294)
    (send p curve-to 446.56 375.6 494.5 456 530.5 537)
    (send p line-to 529 545)

    (send p close)

    p))

(send lam-p scale 1/10 1/10)
(let-values ([(x1 y1 x2 y2) (send lam-p get-bounding-box)])
  (send lam-p translate (- 0 x1 (* (- x2 x1) 6/10)) (- 0 y1 (/ (- y2 y1) 2))))
(send p append lam-p)

(let ([plus (new dc-path%)])
  (define D 15)
  (send plus text-outline (make-font #:size 12 #:family 'swiss) "+" 0 0)
  (send plus translate (- (* 3 D)) (- (* 3 D)))
  (for ([pluses '((- - + + + - -)
                  (- + - - + + -)
                  (+ + + - + + -)
                  (+ + + - + + +)
                  (- + - + - + -)
                  (- - + + + - -)
                  (- - - - - - -))])
    (for ([on (in-list pluses)])
      (when (eq? on '+)
        (send p append plus))
      (send plus translate D 0))
    (send plus translate (- (* 7 D)) D)))

(define RR (* 1.2 R))

(define cover-image-path p)

(define cover-letters-rgb (list 0.4 0.4 0.3))
(define cover-base-rgb (list 0.5 0.5 0.4))
