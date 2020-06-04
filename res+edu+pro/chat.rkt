#lang slideshow
(require racket/draw
         "logo.rkt"
         "balloon.rkt"
         "gear.rkt")

(provide chat-slides
         back-in-1995)

(define (darker color)
  (define c (send the-color-database find-color color))
  (define (d x) (floor (* x 8/10)))
  (make-color (d (send c red))
              (d (send c green))
              (d (send c blue))))
 
(define (make-profile #:talk? [talk? #f])
  (let ([p (new dc-path%)])
    (send p move-to 69 77)
    (send p curve-to
          60 100
          15 100
          6 77)
    (send p arc 0 0 75 100 (* pi -13/16) (* pi 1/8) #f)
    (send p curve-to
          80 50
          80 55
          70 55)
    (send p line-to 70 60)
    (cond
      [talk?
       (send p curve-to
             50 60
             50 70
             69 74)]
      [else
       (send p curve-to
             70 62
             70 73
             69 76)])
    (send p close)
    p))

(define eye
  (let ([p (new dc-path%)])
    (send p ellipse 50 30 10 7)
    p))

(define wide-eye
  (let ([p (new dc-path%)])
    (send p ellipse 50 29 10 9)
    p))

(define blink-line
  (let ([p (new dc-path%)])
    (send p move-to 50 33.5)
    (send p line-to 60 33.5)
    p))

(define pupil
  (let ([p (new dc-path%)])
    (send p ellipse 52 30 6 7)
    p))

(define short-hair
  (let ([p (new dc-path%)])
    (send p arc 10 0 55 20 (* pi -1/6) pi)
    (send p curve-to
          0 10
          0 20
          1 30)
    (send p curve-to
          2 33
          40 30
          40 20)
    (send p close)
    p))

(define spiked-hair
  (let ([p (new dc-path%)])
    (send p arc 10 0 55 20 (* pi -1/6) (* 1/10 pi))
    (send p line-to 60 -2)
    (for ([i (in-range 5 -2 -1)])
      (send p line-to (+ (* i 5) 30) 0)
      (send p line-to (+ (* i 5) 27) -2))
    (send p line-to 20 2)
    (send p line-to 16 0)
    (send p line-to 12 5)
    (send p curve-to
          0 10
          0 20
          1 30)
    (send p curve-to
          2 33
          40 30
          40 20)
    (send p close)
    p))

(define puffy-hair
  (let ([p (new dc-path%)])
    (send p arc 40 0 30 20 (* pi -1/6) (* 5/8 pi))
    (send p arc 25 -3 30 20 (* 1/4 pi) (* 3/4 pi))
    (send p arc 5 0 25 20 (* 1/4 pi) pi)
    (send p curve-to
          -5 10
          -5 20
          1 30)
    (send p curve-to
          -5 33
          -5 37
          0 45)
    (send p arc -5 45 20 10 (* 3/4 pi) (* -1/6 pi))
    (send p curve-to
          9 45
          30 45
          40 20)
    (send p close)
    p))

(define (head #:talk? [talk? #f]
              #:blink? [blink? #f]
              #:eye [eye eye]
              #:hair [hair short-hair]
              #:hair-color [hair-color (darker "brown")]
              #:face [face 'right])
  (define profile
    (make-profile #:talk? talk?))
  (define no-pen (make-pen #:style 'transparent))
  (define no-brush (make-brush #:style 'transparent))
  (define black-pen (make-pen #:width 1))
  (define white-brush (make-brush #:color "white"))
  (define brown-brush (make-brush #:color "brown"))
  (define hair-brush (make-brush #:color hair-color))
  (define body-brush (make-brush #:color "tan"))
  (define p
    (dc (lambda (dc x y)
          (define t (send dc get-transformation))
          (define p (send dc get-pen))
          (define b (send dc get-brush))
          (send dc set-pen no-pen)

          (send dc translate (+ x (if (eq? face 'left) 75 0)) y)
          (when (eq? face 'left)
            (send dc scale -1 1))

          (send dc set-brush body-brush)
          (send dc draw-path profile 0 0)

          (send dc set-brush hair-brush)
          (send dc draw-path hair 0 0)

          (cond
            [(not blink?)
             (send dc set-brush white-brush)
             (send dc draw-path eye 0 0)
             
             (send dc set-brush brown-brush)
             (send dc draw-path pupil 0 0)]
            [else
             (send dc set-brush no-brush)
             (send dc set-pen black-pen)
             (send dc draw-path blink-line 0 0)])

          (send dc set-transformation t)
          (send dc set-pen p)
          (send dc set-brush b))
       75 95))
  (scale p 3))

(define (m-head #:face [face 'left]
                #:talk? [talk? #f])
  (head #:talk? talk?
        #:face face
        #:hair-color "dimgray"
        #:hair spiked-hair))
  

(define (chat-slide #:s-words [s-words #f]
                    #:m-words [m-words #f]
                    #:s-thought [s-thought #f]
                    #:wide-eyes? [wide-eyes? #f]
                    #:blink? [blink? #f]
                    #:timeout [timeout #f])
  (define s (scale (head #:talk? s-words
                         #:hair puffy-hair
                         #:eye (if wide-eyes? wide-eye eye)
                         #:blink? blink?)
                   0.9))
  (define m (m-head #:talk? (or m-words
                                s-thought)))
  (define (pin-words p who words #:thought? [thought? #f])
    (pin-balloon p
                 words
                 who (if (eq? who m) lt-find (if thought? ct-find rt-find))
                 #:thought? thought?
                 #:spike 's
                 #:margin gap-size
                 #:color "gainsboro"
                 #:dy (* (if thought? 4 2) gap-size)
                 #:dx (if (eq? who m) (* 1/2 gap-size) (if thought? 0 (* -1/2 gap-size)))))
  (let* ([p (hc-append (* 9 gap-size) s m)]
         [p (if s-words
                (pin-words p s s-words)
                p)]
         [p (if m-words
                (pin-words p m m-words)
                p)]
         [p (if s-thought
                (pin-words p s s-thought #:thought? #t)
                p)])
    (slide p
           #:timeout timeout)))

(define small-logo (scale logo 1/5))

(define talk-scale 1.3)

(define (back-in-1995)
  (scale (bt "Back in 1995...") talk-scale))

(define (chat-slides)
  (chat-slide)

  (chat-slide
   #:s-words (hc-append gap-size
                        (scale (m-head #:face 'right) 1/4)
                        small-logo
                        (scale (bt "how long?") talk-scale)))

  (define 24-years (hc-append gap-size
                              (scale logo 1/5)
                              (scale (bt "24") 2)
                              (scale (bt "years") talk-scale)))

  (chat-slide
   #:m-words 24-years)

  (for ([i 2])
    (chat-slide
     #:m-words 24-years
     #:wide-eyes? #t
     #:timeout 0.4)
    (chat-slide
     #:m-words 24-years
     #:blink? #t
     #:timeout 0.4))

  (chat-slide
   #:m-words 24-years
   #:wide-eyes? #t)

  (chat-slide
   #:m-words (back-in-1995))

  (chat-slide
   #:s-thought (scale (hc-append (scale (gear) 0.5) (bt " × 1 ?!")) talk-scale)))

(module+ main
  (chat-slides))

