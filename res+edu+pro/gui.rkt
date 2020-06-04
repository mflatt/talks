#lang racket/base
(require racket/runtime-path
         pict)

(provide gui-image
         gui-win-image
         gui-gtk-image)

(define-runtime-path gui-png "gui.png")
(define-runtime-path gui-win-png "gui-win.png")
(define-runtime-path gui-gtk-png "gui-gtk.png")

(define gui-image
  (scale (bitmap gui-png) 0.5))

(define gui-win-image
  (scale (bitmap gui-win-png) 0.5))

(define gui-gtk-image
  (scale (bitmap gui-gtk-png) 0.5))
