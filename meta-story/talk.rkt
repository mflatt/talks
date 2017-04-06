#lang slideshow
(require "title.rkt"
         "bear.rkt"
         "lang-tree.rkt"
         "beginner.rkt"
         "plot.rkt"
         "phase.rkt"
         "hash-lang.rkt"
         "syntax-parse.rkt"         
         "submodule.rkt"
         "scope.rkt"
         "end.rkt")

(title-slide)

;; ----------------------------------------

(part "Goldilocks and the Three Bears")
(goldilocks-slides)
(lang-tree-slides)
(plot-slides)

;; ----------------------------------------

(part "A Tale of Two Languages")
(beginner-slides)

;; ----------------------------------------

(part "A Farewell to Defmacro")
(module+phase-slides)

;; ----------------------------------------

(part "The Importance of Being Some Specific Name")
(hash-lang-slides)

;; ----------------------------------------

(part "Time Enough for Tests")
(submodule-slides)

;; ----------------------------------------

(part "Scope and Sensibility")
(scope-set-slides)

;; ----------------------------------------

(part "A Funny Thing Happened on the Way to the Expansion")
(syntax-parse-slides)

;; ----------------------------------------

(part "To Be Continued...")
(end-slides)
