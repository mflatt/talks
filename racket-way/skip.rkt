#lang slideshow

(provide begin-or-skip)

(define-syntax-rule (begin-or-skip e ...)
  (if condense?
      (begin (skip-slides (begin 'e 1)) ...)
      (begin e ...)))
