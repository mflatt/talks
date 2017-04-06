#lang slideshow
(require "scripting.rkt"
         "real-deps.rkt"
         "problem.rkt"
         "logo.rkt"
         "design.rkt"
         "submod.rkt"
         "docs.rkt"
         "types.rkt"
         "runtime-paths.rkt"
         "split.rkt"
         "papers.rkt"
         "conclusion.rkt"
         "no-projects.rkt"
         "use-lex-scope.rkt"
         "part.rkt"
         "learning.rkt"
         "meaning.rkt"
         "replay.rkt"
         "config.rkt"
         lang-slide)

(define outline
  (make-outline
   'deps "REPL vs. Reason" #f
   '(mods stx phase submod)
   "Metaprogramming in Racket"
   (lambda (which)
     (define (b s)
       (colorize (arrowhead (* gap-size 3/4) 0) (if (eq? s which) "blue" "gray")))
     (vc-append
      (* 4 (current-line-sep))
      (subitem #:bullet (b 'mods) "Modules and phases")
      (subitem #:bullet (b 'stx) "Syntax objects")
      (subitem #:bullet (b 'phase) "Phases, scope, and state")
      (subitem #:bullet (b 'submod) "Submodules")))
   'doc "Putting it Together: Documentation" #f))

(define (person s where) 
  (vc-append
   (current-line-sep)
   (colorize (t s) "blue")
   (scale (t where) 0.8)))

(slide
 (cond
  [(and short-title? gpce-version?) (vc-append
                                     (* 2 (current-line-sep))
                                     (titlet "Submodules in Racket")
                                     (scale (titlet "(Metaprogramming Time!)") 0.75))]
  [gpce-version?
   (vc-append (* 2 (current-line-sep))
              (titlet "Submodules in Racket")
              (scale (hbl-append (t "You Want it ") (it "When") (t ", Again?")) 0.8))]
  [else
   (titlet "Metaprogramming Time!")])
 (blank)
 (scale racket-logo 0.5)
 (blank)
 (if short-title?
     (blank)
     (person "Matthew Flatt"
             "PLT and University of Utah")))

(when printing?
  (meaning-slides)
  (meaning-example-slides))

(when gpce-version? ; later in non-GPCE
  (test-submod-slides)
  (types-slides)
  (runtime-path-slides)
  (short-doc-slides)
  (submodule-split-slides)
  (papers-slides))

;; ----------------------------------------

(unless gpce-version?
  (part-1-slide))

#;
(slide
 #:title "Languages in the Racket Distribution"
 (blank)
 (langs-pict #t #:fit? #t))

(unless gpce-version?
  (story-slides))

(unless gpce-version?
  (use-slides)
  (eval-when-slides))

(phase-slides)

(unless gpce-version?
  (movie-ffi-slides)
  (movie-class-slides))

(unless gpce-version?
  (use-lexical-scope-slides #:escapes? (not gpce-version?)))

(unless gpce-version? ; earlier in GPCE
  (test-submod-slides))

;; ----------------------------------------

(unless gpce-version?
  (part-2-slide))

(if printing?
    (replay-slides)
    (if gpce-version?
        (short-design-slides)
        (design-slides)))

(unless gpce-version?
  (doc-slides))

(unless gpce-version?
  (no-projects-slides))

;; ----------------------------------------

(unless gpce-version?
  (learning-slides))

(end-slides)
