#lang racket/base
;; lightweight module to use for examples
(provide (except-out (all-from-out racket/base)
                     sleep
                     current-seconds)
         (rename-out [fake-sleep sleep]
                     [fake-current-seconds current-seconds]))

(define time (current-seconds))
(define (fake-sleep s)
  (set! time (+ time s)))
(define (fake-current-seconds)
  time)
