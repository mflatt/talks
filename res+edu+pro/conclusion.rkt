#lang slideshow
(require slideshow/play
         "lop.rkt"
         "rhombus-logo.rkt"
         "venn.rkt"
         "person.rkt"
         "gear.rkt")

(provide conclusion-slides)

(define (conclusion-slides)
  (define make (lop-slides #:lang-gear-maker? #t))

  (define rhombus (scale rhombus-logo 1/4))

  (define name "Conclusion")

  (define the-prof (scale (professor) 2))
  (define the-venn (scale (make-venn #:icons? #f) 1/2))
  (define the-gear (scale (gear) 2))

  (define (lesson-slide #:gear? [gear? #f]
                        #:venn? [venn? gear?])
    (slide
     #:name name
     (hc-append
      (* 6 gap-size)
      the-prof
      ((if venn? values ghost) the-venn)
      ((if gear? values ghost) the-gear))))

  (lesson-slide)
  (lesson-slide #:venn? #t)
  (lesson-slide #:gear? #t)
    
  (play-n
   #:title "" ; needed for exact alignment with titled slide
   #:name name
   (lambda (n)
     (make #:finale? #t
           #:rhombus-logo rhombus
           #:n5 n)))

  (slide
   #:title ""
   #:name name
   (let ([p (make #:finale? #t
                  #:rhombus-logo rhombus
                  #:n5 1)])
     (refocus (rb-superimpose
               (inset p (* 6 gap-size) (* 6 gap-size))
               (colorize (text "Thanks!"
                               "signpainter"
                               100)
                         "darkred"))
              p))))

(module+ main
  (conclusion-slides))

