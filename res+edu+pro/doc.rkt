#lang racket/base
(require pict
         racket/runtime-path)

(provide doc)

(define-runtime-path doc-png "doc.png")

(define doc (frame (scale (bitmap doc-png) 0.25)))

