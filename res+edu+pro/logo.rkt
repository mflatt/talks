#lang racket/base
(require pict
         racket/runtime-path)

(provide logo)

(define-runtime-path racket-logo-png "logos/racket-logo.png")

(define logo (scale (bitmap racket-logo-png) 0.7))
