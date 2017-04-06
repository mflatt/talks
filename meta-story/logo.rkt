#lang slideshow
(require racket/runtime-path)

(provide racket-logo)

(define-runtime-path racket-logo-png "language-logos/racket-logo.png")

(define racket-logo
  (scale (bitmap racket-logo-png) 0.5))
