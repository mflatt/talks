#lang racket
(require "card.rkt"
         (only-in pict3d combine
                  move-x
                  move-y
                  move-z
                  set-color
                  set-emitted
                  emitted
                  rgba
                  freeze)
         pict3d-die-cut)

(provide pict->pict3d
         card->pict3d)

(define cache (make-hasheq))

(define (card->pict3d c)
  (hash-ref! cache c
             (lambda ()
               (freeze
                (set-emitted
                 (die-cut-path-datum (card-datum c)
                                     #:depth 0.0
                                     #:expected-scale 5)
                 (emitted (/ (card-red c) 255.0)
                          (/ (card-green c) 255.0)
                          (/ (card-blue c) 255.0)
                          1))))))
  
(define (pict->pict3d p)
  (define ps
    (for/list ([oc (in-list (pict->offset-cards p))])
      (move-y (move-x (card->pict3d (vector-ref oc 0))
                      (vector-ref oc 1))
              (- (vector-ref oc 2)))))

  (apply combine
         (for/list ([p (in-list ps)]
                    [i (in-naturals)])
           (move-z p (* i -0.1)))))
