#lang racket/gui
(require pict
         "code.rkt"
         "to-outlines.rkt"
         "plan.rkt"
         "card.rkt")

(define plan
  (script->planneds (default-or-example-script #t)))

(define pos 0)
(define auto? #f)

(define f (new frame%
               [label "Outline"]
               [width 800]
               [height 800]))
(define s (new slider%
               [parent f]
               [label #f]
               [min-value 0]
               [max-value 100]
               [callback (lambda (s e)
                           (set! auto? #f)
                           (send c refresh))]))
(define c
  (new (class canvas%
         (super-new)
         (inherit refresh)
         (define/override (on-char e)
           (case (send e get-key-code)
             [(#\space right)
              (set! auto? #t)
              (send s set-value 0)
              (set! pos (min (sub1 (length plan)) (add1 pos)))
              (refresh)]
             [(left)
              (set! pos (max 0 (sub1 pos)))
              (refresh)])))
       [parent f]
       [paint-callback
        (lambda (c dc)
          (send dc set-pen (make-pen #:style 'transparent))
          (define dt (/ (send s get-value) 100.0))
          (define p (list-ref plan pos))
          (define c+ps (planned-cards+positions p))
          (for ([c+p (in-list (reverse c+ps))])
            (define c (car c+p))
            (define-values (dx dy) ((cdr c+p) dt))
            (send dc set-brush (make-brush #:color (make-color (card-red c)
                                                               (card-green c)
                                                               (card-blue c))))
            (send dc draw-path (card-dc-path c) dx dy))
          (when auto?
            (thread (lambda ()
                      (sleep 0.03)
                      (when auto?
                        (queue-callback
                         (lambda ()
                           (when auto?
                             (define v (send s get-value))
                             (unless (= v 100)
                               (send s set-value (min 100 (+ 3 v)))
                               (send c refresh))))))))))]))

(send f show #t)
