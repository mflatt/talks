#lang slideshow
(require "desktop.rkt"
         "audience.rkt"
         "scope.rkt"
         "terminology.rkt"
         "pico.rkt"
         "branch.rkt")

(module+ main
  (title-slide)
  (audience-slides)
  (scope-slides)
  (terminology-slides)
  (pico-slides)
  (branches-slide))

;; ----------------------------------------

(define lets-build (colorize (rotate (text "Let's Build" '(italic . "SignPainter") 128) (/ pi 16))
                             "firebrick"))
(define bang (colorize (text "!" '(italic . "Myanmar MN") 800) "firebrick"))

(define central
  (vl-append
   (- (* 2 gap-size))
   (lift-above-baseline (colorize (text "Hygienic" "Phosphate" 148) "blue") -10)
   (parameterize ([current-main-font '(bold . "Optima")]
                  [current-font-size 128])
     (colorize
      (vl-append (- (* 2 gap-size))
                 (t "Macro")
                 (let ([p (t "Expander")])
                   (refocus (hbl-append (- (* 3 gap-size))
                                        p
                                        (ghost bang))
                            p)))
      "mediumblue"))))

(define (title-slide)
  (slide
   (cc-superimpose
    (plt-desktop)
    (pin-over
     (pin-under (refocus (vl-append (- (* 4 gap-size))
                                    (ghost lets-build)
                                    (htl-append (* 2 gap-size)
                                                (blank)
                                                (colorize (text "a" '(bold . "Optima") 64) "firebrick")
                                                (refocus (vc-append (* gap-size 3)
                                                                    central
                                                                    (scale (text "Matthew Flatt" '(bold . "Optima") 32) 1.2))
                                                         central)))
                         central)
                bang lt-find
                bang)
     lets-build lt-find
     lets-build))))
