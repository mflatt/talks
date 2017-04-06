#lang racket/base
(require racket/draw
         racket/class)

(provide (all-defined-out))

(define GreenColor "green")
(define DarkGreenColor "forest green")
(define BlueColor "blue")
(define RedColor "red")
(define BeigeColor (make-object color% 255 255 200))

