#lang slideshow
(require "../scribble/talk/movie.ss"
         "../scribble/talk/analogy.ss"
         "../scribble/talk/castle.ss"
         "../scribble/talk/peek.ss"
         lang-slide
         "tower.rkt"
         "features.rkt"
         "key-and-mouse.rkt"
         "title.rkt")

(title-slide
 (titlet "Languages in Racket"))

(features-slides)

(movie-slides (slide->pict (retract-most-recent-slide)))

;; ----------------------------------------

(analogy-slides #:programmer? #f
                #:skip-balloons? #t)

(key-and-mouse-slides)

(analogy-slides #:skip-balloons? #t)

(define analogy-slide (most-recent-slide))

;; ----------------------------------------

(slide (langs-pict #t #:fit? #t))

;; ----------------------------------------

(tower-slides #:details? #t)

(re-slide analogy-slide)

(peek-slides (slide->pict analogy-slide))
