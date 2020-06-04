#lang racket/gui

(define f (new frame%
               [label "Launch"]
               [width 130]
               [height 200]))
(new canvas%
     [parent f])

(send f show #t)


  
 
