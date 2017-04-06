
(module modified (lib "slideshow.ss" "slideshow")
  
  (provide modified-slide)

  (define (modified-slide)
    (slide/center
     (cc-superimpose
      (let ([m (get-margin)])
        (inset (colorize (filled-rectangle (+ m m client-w 2) (+ m m client-h 2)) "black") (- 0 m 1)))
      (parameterize ([current-main-font 'roman])
        (colorize
         (para/c (* 2/3 client-w)
                 "This talk has been modified from its original version."
                 " "
                 "It has been edited for content and formatted to fit your screen.")
         "white"))))))
