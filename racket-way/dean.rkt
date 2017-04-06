#lang slideshow
(require "../scribble/talk/movie.ss"
         "../scribble/talk/analogy.ss"
         "../scribble/talk/castle.ss"
         "../scribble/talk/peek.ss"
         "../mredtalk3/talk.ss"
         "title.rkt"
         "tower.rkt"
         "features.rkt"
         "key-and-mouse.rkt"
         "racket-is.rkt"
         "research.rkt")

(features-slides)

(movie-slides (slide->pict (retract-most-recent-slide)))
                                        

;; ----------------------------------------

(analogy-slides #:programmer? #f
                #:skip-balloons? #t)

(key-and-mouse-slides)

(analogy-slides #:skip-balloons? #t)

;; ----------------------------------------

(slide
 #:title "Languages in the Racket Distribution"
 (blank)
 (scale all-langs (/ client-w (pict-width all-langs))))

;; ----------------------------------------

(research-slides)
