#lang slideshow
(require "../scope-sets/talk2/scope.rkt"
         "../scope-sets/talk2/terminology.rkt"
         "lesson.rkt"
         "utils.rkt")

(provide scope-set-slides)

(define (scope-set-slides)
  (scope-slides #:just-or? #t)
  (terminology-slides
   #:complete-slide (lambda (p) 
                      (slide (add-cite p "Flatt [POPL'16]")))
   #:extra-slides (lambda (p)
                    (define inc
                      (incidental "Use in macros"))
                    (define inev
                      (inevitable #:width (* client-w 0.4)
                                  "Binding resolution interleaved with transformation"))
                    (define (combine inev?)
                      (slide
                       (refocus
                        (vc-append
                         (* gap-size)
                         p
                         (ht-append
                          (* 2 gap-size)
                          inc
                          ((if inev? values ghost) inev)))
                        p)))
                    (combine #f)
                    (combine #t))))

(module+ main
  (scope-set-slides))
