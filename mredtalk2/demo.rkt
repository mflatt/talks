#lang racket/gui

(define f (new frame% [label "Demo"]))

(define fnt
  (make-object font% 24
    'system 'normal 'normal))

(new button%
     [label "Loop Forever"]
     [parent f]
     [font fnt]
     [callback
      (lambda (b e)
        (let loop () (loop)))])

(new button%
     [label "Crash"]
     [parent f]
     [font fnt]
     [callback
      (lambda (b e)
        (+ 'not-a-num 1))])

(display "Frame is ready...")
