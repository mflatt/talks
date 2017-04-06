#lang slideshow
(require "bear.rkt")

(provide contributors-slides
         
         matthias-bear
         shriram-bear
         matthew-bear
         eli-bear
         robby-bear
         jay-bear
         sam-bear
         ryan-bear
         stephen-bear
         make-bear-group
         
         format-bear)

(define (contributors #:scale [s 0.9] . persons)
  (map (lambda (person)
         (scale (hc-append gap-size (cadr person) (t (car person)))
                s))
       persons))

(define (add-thanks p)
  (refocus
   (rt-superimpose p
                   (inset (colorize (rotate (text "Thanks!" '(italic . "SignPainter") 64) (/ pi 16))
                                    "firebrick")
                          0 -100 0 100))
   p))

(define matthias-bear (head #:hair 'yellow-along-top))
(define shriram-bear (head #:hair 'black-parted))
(define matthew-bear (head #:hair 'brown-spiky))
(define eli-bear (head #:hair 'black-vee))
(define robby-bear (head #:hair 'brown-parted))
(define jay-bear (head))
(define sam-bear (head #:hair 'brown-vee))
(define ryan-bear (head #:hair 'brown-middle-parted))
(define stephen-bear (head #:hair 'spiky))

(define (make-bear-group m #:filter [w values] #:sep [sep 0])
  (vc-append
   sep
   (hc-append sep (w matthias-bear) (w shriram-bear) (w robby-bear))
   (hc-append sep (w jay-bear) (w ryan-bear) (w sam-bear) (w m))))

(define (format-bear bear first last)
  (scale (hc-append gap-size
                    bear
                    (vl-append (t first) (t last)))
         0.8))

(define (contributors-slides)
  (slide
   #:title "Some Collaborators"
   (add-thanks
    (table 2
           (contributors
            (list "Matthias Felleisen" matthias-bear)
            (list "Robby Findler" robby-bear)
            (list "Shriram Krishnamurthi" shriram-bear)
            (list "Ryan Culpepper" ryan-bear)
            (list "Sam Tobin-Hochstadt" sam-bear)
            (list "Jay McCarthy" jay-bear)
            (list "Eli Barzilay" eli-bear)
            (list "Christos Dimoulas" (head #:hair 'black-puff))
            (list "Stephen Chang" stephen-bear)
            
            (list "Vincent St-Amour" (head #:hair 'yellow-middle-parted))
            (list "T. Stephen Strickland" (head #:hair 'brown-parted))
            (list "Alex Knauth" (head #:hair 'brown-parted-left))
            (list "Ben Greenman" (head #:hair 'brown-parted-left))
            (list "Jon Rafkind" (head #:hair 'brown-parted-left)))
           lc-superimpose lc-superimpose
           (* 2 gap-size) gap-size))))
   

(module+ main
  (contributors-slides))
