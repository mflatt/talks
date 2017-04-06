#lang racket/base
(require racket/gui/base
         racket/class
         "movie-panel.rkt")

(define f (new frame%
               [label "Stay Functional"]
               [width 800]
               [height 600]))

(define mp (new movie-panel% 
                [parent f]
                [file "stay-functional.mp4"]))

(define m (new message%
               [label ""]
               [stretchable-width #t]
               [parent f]))

(send mp set-current-time 12000)

(send f show #t)

(void (thread (lambda ()
                (let loop ()
                  (sleep 0.1)
                  (when (send f is-shown?)
                    (define now (send mp get-current-time))
                    (send m set-label (format "~a" now))
                    (loop))))))
