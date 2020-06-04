#lang slideshow
(require racket/draw
         racket/class
         "util.rkt"
         "paper.rkt"
         "book.rkt"
         "person.rkt"
         "gear.rkt")

(provide as-history
         ca
         leftward
         make-slide-assembler)

(define-syntax as-history
  (syntax-rules ()
    [(as-history #:res res #:edu edu #:prod prod body ...)
     (call-as-history #:res res #:edu edu #:prod prod (lambda () body ...))]
    [(as-history #:res res #:prod prod body ...)
     (call-as-history #:res res #:prod prod (lambda () body ...))]
    [(as-history #:edu edu #:prod prod body ...)
     (call-as-history #:edu edu #:prod prod (lambda () body ...))]
    [(as-history #:res res #:edu edu body ...)
     (call-as-history #:res res #:edu edu (lambda () body ...))]
    [(as-history #:res res body ...)
     (call-as-history #:res res (lambda () body ...))]
    [(as-history #:edu edu body ...)
     (call-as-history #:edu edu (lambda () body ...))]
    [(as-history #:prod prod body ...)
     (call-as-history #:prod prod (lambda () body ...))]
    [(as-history body ...)
     (call-as-history (lambda () body ...))]))

(define the-paper (delay (paper)))
(define the-paper+dissertation
  (delay (refocus (lt-superimpose
                   (force the-paper)
                   (inset (book (t "PhD")) 0 gap-size 0 0))
                  (force the-paper))))
(define the-students (delay (inset (scale (students) 0.5) 0 5 0 0)))
(define the-gear (delay (gear)))

(define (halo p n)
  (if (n . >= . 1)
      (cc-superimpose
       (bright-box (pict-width p) (pict-height p))
       p)
      p))

(define (call-as-history #:res [res 1.0]
                         #:edu [edu 1.0]
                         #:prod [prod 1.0]
                         thunk)
  (parameterize ([current-slide-assembler (make-slide-assembler (current-slide-assembler)
                                                                #:res res
                                                                #:edu edu
                                                                #:prod prod)])
    (thunk)))

(define (make-slide-assembler orig
                              #:res [res 1.0]
                              #:edu [edu 1.0]
                              #:prod [prod 1.0])
  (define the-research (if (res . > . 1)
                           the-paper+dissertation
                           the-paper))
  (lambda (name title-sep content)
    (define p (orig name title-sep content))
    (rc-superimpose
     (ct-superimpose p
                     full-page)
     (vc-append
      (* 4 gap-size)
      (over-cellophane (halo (force the-research) res) (max 0.2 (min 1 res)))
      (over-cellophane (halo (force the-students) edu) (max 0.2 edu))
      (over-cellophane (inset (halo (inset (force the-gear) -10) prod) 10) (max 0.2 prod))))))

(define (bright-box w h)
  (define M 20)
  (define no-pen (make-pen #:style 'transparent))
  (dc (lambda (dc x y)
        (define cx (+ x (/ w 2)))
        (define cy (+ y (/ h 2)))
        (define p (send dc get-pen))
        (define b (send dc get-brush))
        (send dc set-pen no-pen)
        (send dc set-brush (make-brush
                            #:gradient (make-object radial-gradient%
                                                    cx cy
                                                    (/ (min w h) 2)
                                                    cx cy
                                                    (+ M (/ (min w h) 2))
                                                    (list (list 0 (make-color 255 255 0 1.0))
                                                          (list 1 (make-color 255 255 0 0.0))))))
        (send dc draw-rectangle (- x M) (- y M) (+ w (* 2 M)) (+ h (* 2 M)))
        (send dc set-pen p)
        (send dc set-brush b))
      w h))
                                    
(define (ca year s #:who [who ""])
  (define main (if (pict? s) s (titlet s)))
  (refocus (hbl-append (* 3 gap-size)
                       (scale (t (format "ca. ~a" year)) 0.8)
                       main
                       (scale (it who) 0.8))
           main))

(define (leftward p) (inset p 0 0 (* client-w 1/8) 0))
