#lang slideshow
(require "movie.ss"
         "princess.ss"
         "history.ss"
         "castle.ss"
         "analogy.ss"
         "at-exp.ss"
         "phases.ss"
         "plt.ss"
         "end.ss"
         "peek.ss")

(define (person s where) 
  (vc-append
   (current-line-sep)
   (colorize (t s) "blue")
   (scale (t where) 0.8)))

(define (title-slide the-title the-authors)
  (define title (vc-append
                 (* gap-size 4)
                 the-title
                 plt-bm
                 the-authors))
  (cc-superimpose full-page
                  title))

(movie-slides (title-slide
               (vc-append
                (* 2 (current-line-sep))
                (titlet "Scribble")
                (scale
                 (titlet "Closing the Book on Ad Hoc Documentaton Tools")
                 0.8))
               (hbl-append
                (* 3 gap-size)
                (person "Matthew Flatt" "University of Utah")
                (person "Eli Barzilay" "Northeastern University")
                (person "Robert Bruce Findler" "Northwestern University"))))
(analogy-slides)
(slide #:name "Part 0: Princess" (scale (make-princess) 2.0))
(history-slides)
(slide #:name "Part 1: Castle" castle) (slide)
(slide #:name "Part 2: Well" million-well) (slide)
(at-exp-slides) (slide)
(phases-slides) (slide)
(slide #:name "Part 3: Kingdom" kingdom)
(analogy-again-slides)
(plt-slides) (slide)
(end-slides)
(peek-slides final-end-slide)
