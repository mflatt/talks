#lang s-exp "base.rkt"
;; lightweight module to use for examples
(require (for-syntax "base.rkt"))
(provide (all-from-out "base.rkt")
         (for-syntax (all-from-out "base.rkt")))
