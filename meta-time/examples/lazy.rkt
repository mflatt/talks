#lang lazy

(define ones (cons 1 ones))

(list-ref ones 42)

