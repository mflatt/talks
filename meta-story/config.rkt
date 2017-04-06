#lang slideshow
(require slideshow/balloon
         slideshow/code)

(balloon-enable-3d #f)

(current-keyword-list (append (current-keyword-list)
                              '("defun" "setq" "eval-when"
                                ":compile-toplevel" ":load-toplevel")))
