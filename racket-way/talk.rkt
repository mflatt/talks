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
         slideshow/balloon)

(title-slide
 (titlet "The Racket Way"))

(features-slides)

(movie-slides (slide->pict (retract-most-recent-slide)))

(peek-slides (slide->pict (most-recent-slide))
             #:wave? #f
             #:smile? #f
             #:balloon (lambda (p)
                         (pin-balloon
                          (wrap-balloon (para #:fill? #f
                                              "I'm confused... are we still talking about"
                                              (bt "Racket") "?")
                                        'nw (- gap-size) 0)
                          p
                          p cc-find)))
                                        

;; ----------------------------------------

(analogy-slides #:programmer? #f
                #:skip-balloons? #t)

(key-and-mouse-slides)

(analogy-slides #:skip-balloons? #t)

;; ----------------------------------------

(racket-way-slides)

;; ----------------------------------------

(slide
 #:title "Everything is a Program"
 castle)

;; Scribble, Slideshow, setup/infotab

;; ----------------------------------------

(slide
 #:title "Concepts as Language Constructs"
 million-well)

(mred-slides)

;; ----------------------------------------

(slide
 #:title "Language Extensibility"
 kingdom)

(slide
 #:title "Languages in the Racket Distribution"
 (blank)
 all-langs)

(tower-slides)

;; ----------------------------------------

(racket-is-slides)

(unless condense?
  (peek-slides (slide->pict (most-recent-slide))))

(current-process-milliseconds)
