#lang racket/gui
(require pict
         "code.rkt"
         "to-outlines.rkt")

(define picts
  (append
   (default-or-example #f)
   (basic-lexical-scope)
   (scopes-example)
   (default-or-example #t)))

(define pos 0)

(define f (new frame%
               [label "Outline"]
               [width 800]
               [height 800]))
(new (class canvas%
       (super-new)
       (inherit refresh)
       (define/override (on-char e)
         (case (send e get-key-code)
           [(#\space right)
            (set! pos (min (sub1 (length picts)) (add1 pos)))
            (refresh)]
           [(left)
            (set! pos (max 0 (sub1 pos)))
            (refresh)])))
     [parent f]
     [paint-callback
      (lambda (c dc)
        (define rdc (new record-dc%))
        (draw-pict (list-ref picts pos) rdc 0 0)
        (define outlines (reverse (record-dc->outlines rdc)))
        (send dc set-pen (make-pen #:style 'transparent))
        (for ([o (in-list outlines)])
          (send dc set-brush (make-brush #:color (outline-color o)))
          (send dc draw-path (outline-path o) 0 0)))])

(send f show #t)
