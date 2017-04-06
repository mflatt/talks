#lang slideshow
(require slideshow/play
         racket/draw
         "utils.rkt")

(provide goldilocks-slides
         many-desks-slides

         head
         hot-papa-bowl
         gold-brush)

(define brown (make-color 182 100 76))

(define no-brush (make-brush #:style 'transparent))
(define white-brush (make-brush #:color "white"))
(define black-brush (make-brush #:color "black"))
(define red-brush (make-brush #:color "red"))
(define gray-brush (make-brush #:color "gray"))
(define dark-gray-brush (make-brush #:color "darkgray"))
(define pink-brush (make-brush #:color "pink"))
(define semi-white-brush (make-brush #:color (make-color 255 255 255 0.5)))
(define brown-brush (make-brush #:color brown))
(define tan-brush (make-brush #:color "tan"))
(define med-brown-brush tan-brush #;(make-brush #:color "darkgoldenrod"))
(define peach-brush (make-brush #:color "peachpuff"))
(define light-brown-brush (make-brush #:color (brighter-color brown 2)))
(define red-brown-brush (make-brush #:color (make-color 240 100 76)))
(define gold-brush (make-brush #:color "gold"))
(define yellow-brush (make-brush #:color "yellow"))
(define blue-brush (make-brush #:color "lightblue"))
(define green-brush (make-brush #:color "lightgreen"))
(define dark-blue-brush (make-brush #:color "darkblue"))
(define almost-black-brush (make-brush #:color (make-color 50 50 50)))
(define lightest-tan-brush (make-brush #:color "NavajoWhite"))
(define light-tan-brush (make-brush #:color "Burlywood"))

(define black-pen (make-pen #:width 1))
(define brown-pen (make-pen #:color "brown"))
(define no-pen (make-pen #:style 'transparent))
(define gray-pen (make-pen #:color "gray" #:width 2))
(define dark-gray-pen (make-pen #:color (make-color 80 80 80)))
(define dark-gray-thin-pen (make-pen #:color (make-color 80 80 80) #:width 0.25))
(define semi-white-pen (make-pen #:color (make-color 150 150 150 0.5) #:width 1))
(define thick-black-pen (make-pen #:width 2))
(define fat-black-pen (make-pen #:width 20))
(define fat-brown-pen (make-pen #:width 20 #:color "brown"))

(define papa-brush blue-brush)
(define mama-brush pink-brush)
(define baby-brush green-brush)
(define dress-brush yellow-brush)

(define-syntax-rule (with-state dc x y body ...)
  (let ([p (send dc get-pen)]
        [b (send dc get-brush)]
        [t (send dc get-transformation)])
    (send dc translate x y)
    body ...
    (send dc set-pen p)
    (send dc set-brush b)
    (send dc set-transformation t)))

(define spike
  (let ([p (new dc-path%)])
    (send p move-to 0 20)
    (send p line-to 5 0)
    (send p line-to 10 20)
    (send p close)
    p))

(define curly-q
  (let ([p (new dc-path%)])
    (send p move-to 0 20)
    (send p curve-to -10 20 -10 0 0 0)
    (send p curve-to 20 10 20 30 0 30)
    (send p curve-to -10 30 -20 15 -10 15)
    (send p curve-to -10 15 -5 25 0 25)
    (send p curve-to 10 30 10 10 0 5)
    (send p curve-to -3 5 -3 10 -3 10)
    (send p close)
    (send p rotate (* pi 1/4))
    (send p translate -10 0)
    p))

(define swoop
  (let ([p (new dc-path%)])
    (send p move-to 50 0)
    (send p curve-to -10 0 -10 60 -10 80)
    (send p curve-to -10 90 -20 80 -20 75)
    (send p curve-to -30 80 -20 90 -10 90)
    (send p curve-to 20 90 15 60 50 50)
    (send p close)
    (let ([p2 (new dc-path%)])
      (send p2 append p)
      (send p2 scale -1 1)
      (send p2 translate 100 0)
      (send p append p2))
    p))

(define parted
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p curve-to 30 20 60 20 60 13)
    (send p curve-to 65 20 70 20 75 12)
    (send p close)
    p))

(define parted-left
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p curve-to 30 15 35 15 40 13)
    (send p curve-to 40 20 70 20 75 12)
    (send p close)
    p))

(define middle-parted
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p curve-to 30 20 40 20 50 13)
    (send p curve-to 60 20 70 20 75 12)
    (send p close)
    p))

(define puff
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p curve-to 30 20 40 20 40 13)
    (send p curve-to 45 20 60 20 60 13)
    (send p curve-to 65 20 70 20 75 12)
    (send p close)
    p))

(define along-top
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p arc 30 12 40 8 pi 0 #f)
    (send p close)
    p))

(define vee
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p curve-to 30 15 45 15 50 20)
    (send p curve-to 55 15 70 15 75 12)
    (send p close)
    p))

(define chef-hat
  (let ([p (new dc-path%)])
    (send p move-to 0 35)
    (send p curve-to 25 45 25 45 50 35)
    (send p line-to 50 0)
    (send p curve-to 60 0 70 -30 50 -20)
    (send p curve-to 60 -40 20 -30 25 -20)
    (send p curve-to 30 -30 -10 -40 0 -20)
    (send p curve-to -20 -30 -10 0 0 0)
    (send p close)
    p))

(define chef-hat-line
  (let ([p (new dc-path%)])
    (send p move-to 0 15)
    (send p curve-to 25 25 25 25 50 15)
    p))
    
(define wizard-hat
  (let ([p (new dc-path%)])
    (send p move-to 50 -30)
    (send p line-to 20 20)
    (send p curve-to 40 30 60 30 80 20)
    (send p close)
    p))

(define star
  (let ([p (new dc-path%)])
    (define delta (/ pi 5))
    (define in 1/3)
    (send p move-to 1 0)
    (for/fold ([angle delta])
              ([point (in-range 5)])
      (send p line-to (* in (cos angle)) (* in (sin angle)))
      (define new-angle (+ angle delta))
      (send p line-to (cos new-angle) (sin new-angle))
      (+ new-angle delta))
    (send p close)
    (send p rotate (/ pi 10))
    (send p scale (/ 30 2) (/ 30 2))
    (send p translate 50 0)
    p))

(define spiral
  (let ([p (new dc-path%)])
    (send p move-to 0 0)
    (send p arc 0 -2 4 4 pi (* 2 pi))
    (send p arc -4 -4 8 8 0 pi)
    (send p arc -4 -6 12 12 pi (* 2 pi))
    (send p arc -8 -8 16 16 0 (* 5/4 pi))
    p))

(define grin
  (let ([p (new dc-path%)])
    (send p arc 0 -10 24 20 (* pi -7/8) (* pi -1/8))
    (send p arc 0 -5 24 10 (* pi -1/8) (* pi -7/8) #f)
    (send p close)
    p))

(define big-grin
  (let ([p (new dc-path%)])
    (send p append grin)
    (send p scale 1.6 1.6)
    p))

(define goatee
  (let ([p (new dc-path%)])
    (send p arc 0 0 40 20 0 pi)
    (send p line-to 20 40)
    (send p close)
    (send p ellipse 5 5 30 10)
    p))

(define (make-eyelid from to)
  (let ([p (new dc-path%)])
    (send p arc 0 0 25 20 (* pi from) (* pi to) #f)
    (send p close) 
    p))

(define eyelid (make-eyelid 7/8 1/8))
(define high-eyelid (make-eyelid 6/8 2/8))
(define low-eyelid (make-eyelid 9/8 -1/8))
(define pos-eyelid (make-eyelid 1 1/8))
(define neg-eyelid (make-eyelid 7/8 0))
(define high-pos-eyelid (make-eyelid 7/8 2/8))
(define high-neg-eyelid (make-eyelid 6/8 1/8))
(define bottom-neg-eyelid (make-eyelid 14/8 9/8))
(define bottom-pos-eyelid (make-eyelid 15/8 10/8))

(define (head #:hair [hair #f]
              #:bear? [bear? #t]
              #:eyes [eyes 'normal]
              #:mouth [mouth 'normal]
              #:hair-brush [hair-brush red-brown-brush])
  (define face-brush (if bear? brown-brush peach-brush))
  (define face-pen dark-gray-pen)
  (dc (lambda (dc x y)
        (with-state dc
          x y
          (case hair
            [(swoop)
             (send dc set-pen no-pen)
             (send dc set-brush hair-brush)
             (send dc draw-path swoop 0 0)])
          (send dc set-pen face-pen)
          (send dc set-brush face-brush)
          (when bear?
            (send dc draw-ellipse 0 0 30 30)
            (send dc draw-ellipse 70 0 30 30))
          (send dc draw-ellipse 0 10 100 75)
          (send dc set-brush white-brush)
          (send dc set-pen no-pen)
          (send dc draw-ellipse 20 30 25 20)
          (send dc draw-ellipse 55 30 25 20)
          (send dc set-brush black-brush)
          (case eyes
            [(rolled)
             (send dc draw-ellipse 28 30 10 10)
             (send dc draw-ellipse 62 30 10 10)]
            [(normal angry worried wide kinda-wide tired cry)
             (send dc draw-ellipse 28 35 10 10)
             (send dc draw-ellipse 62 35 10 10)
             (send dc set-pen face-pen)
             (send dc set-brush face-brush)
             (case eyes
               [(normal)
                (send dc draw-path eyelid 20 30)
                (send dc draw-path eyelid 55 30)]
               [(tired)
                (send dc draw-path low-eyelid 20 30)
                (send dc draw-path low-eyelid 55 30)]
               [(kinda-wide)
                (send dc draw-path high-eyelid 20 30)
                (send dc draw-path high-eyelid 55 30)]
               [(angry)
                (send dc draw-path neg-eyelid 20 30)
                (send dc draw-path pos-eyelid 55 30)]
               [(worried)
                (send dc draw-path high-pos-eyelid 20 30)
                (send dc draw-path high-neg-eyelid 55 30)]
               [(cry)
                (send dc draw-path high-pos-eyelid 20 30)
                (send dc draw-path high-neg-eyelid 55 30)
                (send dc draw-path bottom-pos-eyelid 20 30)
                (send dc draw-path bottom-neg-eyelid 55 30)])]
            [(dazed)
             (send dc set-pen black-pen)
             (send dc set-brush no-brush)
             (send dc draw-path spiral 33 40)
             (send dc draw-path spiral 67 40)])
          (send dc set-pen no-pen)
          (send dc set-brush black-brush)
          (when bear?
            (send dc draw-ellipse 38 50 24 20))
          (send dc set-pen face-pen)
          (send dc set-brush no-brush)
          (unless bear?
            (send dc draw-arc 46 40 8 20 (* pi -7/8) (* pi -1/8)))
          (case mouth
            [(normal)
             (unless bear?
               (send dc draw-arc 38 50 24 20 (* pi -7/8) (* pi -1/8)))]
            [(frown)
             (when bear?
               (send dc set-pen thick-black-pen))
             (send dc draw-arc 38 (if bear? 72 64) 24 20 (* pi 1/8) (* pi 7/8))]
            [(straight)
             (when bear?
               (send dc set-pen thick-black-pen))
             (send dc draw-line 38 (if bear? 73 65) 62 (if bear? 73 65))])
          (send dc set-pen no-pen)
          (case mouth
            [(hot)
             (send dc set-brush white-brush)
             (send dc draw-ellipse 38 65 24 10)]
            [(grin)
             (send dc set-brush white-brush)
             (send dc draw-path grin 38 (if bear? 70 60))]
            [(big-grin)
             (send dc set-brush white-brush)
             (send dc draw-path big-grin 31 (if bear? 65 60))]
            [(goatee)
             (send dc set-brush black-brush)
             (send dc draw-path goatee 30 70)])
          (case hair
            [(spiky brown-spiky)
             (case hair
               [(brown-spiky) (send dc set-brush med-brown-brush)]
               [else (send dc set-brush black-brush)])
             (for ([i (in-range 30 61 5)])
               (send dc draw-path spike i 0))]
            [(yellow-parted brown-parted brown-parted-left black-parted 
                            yellow-middle-parted brown-middle-parted
                            yellow-puff brown-puff black-puff
                            yellow-along-top brown-along-top black-along-top
                            brown-vee black-vee)
             (send dc set-brush (case hair
                                  [(yellow-parted yellow-middle-parted yellow-puff yellow-along-top) gold-brush]
                                  [(brown-parted brown-parted-left brown-middle-parted brown-puff brown-along-top brown-vee) med-brown-brush]
                                  [(black-parted black-puff black-along-top black-vee) black-brush]))
             (case hair
               [(yellow-parted brown-parted black-parted)
                (send dc draw-path parted)]
               [(brown-parted-left)
                (send dc draw-path parted-left)]
               [(yellow-middle-parted brown-middle-parted)
                (send dc draw-path middle-parted)]
               [(yellow-puff brown-puff black-puff)
                (send dc draw-path puff)]
               [(yellow-curly brown-curly black-curly)
                (for ([i 8])
                  (send dc draw-ellipse (+ 27 (* 5 i)) 5 10 10))]
               [(yellow-along-top brown-along-top black-along-top)
                (send dc draw-path along-top)]
               [(brown-vee black-vee)
                (send dc draw-path vee)])]
            [(swoop)
             (send dc set-brush hair-brush)
             (send dc draw-ellipse 20 5 60 15)]
            [(curly-q)
             (send dc set-brush light-brown-brush)
             (send dc draw-path curly-q 50 5)]
            [(chef)
             (send dc set-brush white-brush)
             (send dc set-pen dark-gray-pen)
             (send dc draw-path chef-hat 25 -20)
             (send dc set-brush no-brush)
             (send dc draw-path chef-hat-line 25 -20)]
            [(wizard wizard+star)
             (send dc set-brush dark-blue-brush)
             (send dc draw-path wizard-hat 0 0)
             (case hair
               [(wizard+star)
                (send dc set-brush gold-brush)
                (send dc draw-path star 0 0)])])))
      100
      75))

(define torso
  (dc (lambda (dc x y)
        (with-state dc
          x y
          (send dc set-pen dark-gray-pen)
          (send dc set-brush brown-brush)
          (send dc draw-ellipse 0 0 100 150)))
      100 150))

(define (dress #:bend [bend 0])
  (let ([p (new dc-path%)])
    (define (b n) (- n bend))
    (define (b/2 n) (- n (/ bend 2)))
    (send p move-to (b 0) 150)
    (send p curve-to (b 8) 150 (b/2 10) 100 (b/2 15) 75)
    (send p curve-to 8 0 30 0 40 0)
    (send p line-to 60 0)
    (send p curve-to 70 0 90 0 85 75)
    (send p curve-to 95 150 (b 92) 150 (b 100) 150)
    (send p close)
    (dc (lambda (dc x y)
          (with-state dc
            x y
            (send dc set-pen dark-gray-pen)
            (send dc set-brush dress-brush)
            (send dc draw-path p 0)))
        100 150)))

(define (bear-arm #:rotate rot)
  (dc (lambda (dc x y)
        (with-state dc
          x y
          (send dc translate 12.5 12.15)
          (send dc rotate rot)
          (send dc set-pen dark-gray-pen)
          (send dc set-brush brown-brush)
          (send dc draw-ellipse -12.5 -12.5 25 80)))
      25 80))

(define bear-leg
  (dc (lambda (dc x y)
        (with-state dc
          x y
          (send dc set-pen dark-gray-pen)
          (send dc set-brush brown-brush)
          (send dc draw-ellipse 0 0 40 80)))
      40 80))

(define (girl-arm #:rotate rot
                  #:bend? [bend? #f])
  (define bend-p (new dc-path%))
  (send bend-p move-to -12.5 0)
  (send bend-p line-to 12.5 0)
  (send bend-p line-to 12.5 35)
  (send bend-p curve-to 12.5 40 12.5 40 -20 80)
  (send bend-p curve-to -28 90 -28 90 -35 70)
  (send bend-p curve-to -40 60 -14 30 -15.5 25)
  (send bend-p close)
  (dc (lambda (dc x y)
        (with-state dc
          x y
          (send dc translate 12.5 12.15)
          (send dc rotate rot)
          (send dc set-pen dark-gray-pen)
          (send dc set-brush peach-brush)
          (cond
           [bend?
            (send dc draw-path bend-p 0 0)]
           [else
            (send dc draw-ellipse -12.5 -12.5 25 100)])
          (send dc set-brush dress-brush)
          (send dc draw-ellipse -15 -15 30 30)))
      25 100))

(define girl-leg
  (dc (lambda (dc x y)
        (with-state dc
          x y
          (send dc set-pen dark-gray-pen)
          (send dc set-brush peach-brush)
          (send dc draw-ellipse 0 0 25 80)))
      25 80))

(define spoon
  (let ([p (new dc-path%)])
    (send p move-to -0.5 0)
    (send p line-to -0.5 8)
    (send p curve-to -2 8 -2 12 0 12)
    (send p curve-to 2 12 2 8 0.5 8)
    (send p line-to 0.5 0)
    (send p close)
    (scale
     (dc (lambda (dc x y)
           (with-state dc
             x y
             (send dc set-pen no-pen)
             (send dc set-brush dark-gray-brush)
             (send dc draw-path p -1 0)))
         2 12)
     4)))

(define (body head torso left-arm right-arm left-leg right-leg
              #:arm-squeeze [arm-squeeze 0]
              #:arm-shift [arm-shift 20]
              #:spoon? [spoon? #f])
  (define g-head (ghost head))
  (define g-torso (ghost torso))
  (define legs (hc-append 20 left-leg right-leg))
  (define g-legs (ghost legs))
  (define (up p) (inset p 0 (- arm-shift) 0 arm-shift))
  (pin-over (pin-under
             (pin-under
              (vc-append -10
                         g-head
                         (vc-append
                          -30
                          (hc-append (- -30 arm-squeeze)
                                     (up left-arm)
                                     g-torso
                                     (up right-arm))
                          g-legs))
              torso lt-find
              torso)
             legs lt-find
             legs)
            head lt-find
            head))

(define (add-spoon p)
  (pin-over p
            (* -0.7 (pict-height p)) (- 0 (pict-height spoon))
            (rotate spoon pi)))

(define (bear head bear-scale #:spoon? [spoon? #f])
  (scale (body head torso
               ((if spoon? add-spoon values) (bear-arm #:rotate (* pi (if spoon? -5/8 -1/8))))
               (bear-arm #:rotate (* pi 1/8))
               bear-leg bear-leg
               #:spoon? spoon?)
         bear-scale))

(define papa-scale 1.3)
(define mama-scale 1.1)
(define baby-scale 1.0)

(define papa-hair 'spiky)
(define mama-hair 'swoop)
(define baby-hair 'curly-q)

(define table
  (let ([p (new dc-path%)])
    (send p move-to -80 -50)
    (send p line-to 80 -50)
    (send p line-to 110 30)
    (send p line-to -110 30)
    (send p close)
    (dc (lambda (dc x y)
          (with-state dc
            x (- y 30)
            (send dc set-pen no-pen)
            (send dc set-brush brown-brush)
            (send dc draw-rectangle -100 40 10 80)
            (send dc draw-rectangle 90 40 10 80)
            (send dc set-brush red-brush)
            (send dc draw-path p 0 0)
            (send dc draw-rectangle -110 30 220 20)
            (send dc set-brush semi-white-brush)
            (for ([i (in-range -70 80 12)])
              (define x1 (- i 3))
              (define x2 (+ i 3))
              (define x3 (* (/ 220 160) x2))
              (define x4 (* (/ 220 160) x1))
              (send dc draw-polygon (list (cons x1 -50)
                                          (cons x2 -50)
                                          (cons x3 30)
                                          (cons x4 30))
                    0 0)
              (send dc draw-rectangle x4 30 (- x3 x4) 20))
            (for ([i (in-range -50 30 20)])
              (define tx0 (+ -80 (* -30 (/ (- i -50) 80))))
              (define tx1 (+ 80 (* 30 (/ (- i -50) 80))))
              (define bx0 (+ -80 (* -30 (/ (- (+ i 10) -50) 80))))
              (define bx1 (+ 80 (* 30 (/ (- (+ i 10) -50) 80))))
              (send dc draw-polygon (list (cons tx0 i)
                                          (cons tx1 i)
                                          (cons bx1 (+ i 10))
                                          (cons bx0 (+ i 10)))
                    0 0))))
        0 30)))

(define (bowl #:color [bowl-brush white-brush]
              #:heat heat
              #:empty? [empty? #f])
  (scale
   (let ([p (new dc-path%)]
         [sp (new dc-path%)])
     ;; Bowl bottom
     (send p move-to 0 5)
     (send p curve-to 5 20 5 20 10 20)
     (send p curve-to 15 20 15 20 20 5)
     (send p close)
     ;; Steam
     (send sp move-to 0 0)
     (send sp curve-to 3 -3 3 -3 0 -5)
     (send sp curve-to -3 -8 -3 -8 0 -10)
     (dc (lambda (dc x y)
           (with-state dc
             x y
             (send dc set-pen dark-gray-thin-pen)
             (send dc set-brush bowl-brush)
             (send dc draw-path p 0 0)
             (send dc set-brush (if empty? white-brush tan-brush))
             (send dc draw-ellipse 0 0 20 10)
             (send dc set-pen no-pen)
             (send dc set-pen semi-white-pen)
             (send dc set-brush no-brush)
             (when (heat . > . 0)
               (when (heat . > . 1)
                 (send dc draw-path sp 5 2))
               (send dc draw-path sp 10 2)
               (when (heat . > . 1)
                 (send dc draw-path sp 15 2)))))
         20 20))
   4 3))

(define hot-papa-bowl (bowl #:color papa-brush #:heat 2))
(define hot-mama-bowl (bowl #:color mama-brush #:heat 2))
(define hot-baby-bowl (bowl #:color baby-brush #:heat 2))
(define mama-cold-bowl (bowl #:color mama-brush #:heat 0))
(define baby-just-right-bowl (bowl #:color baby-brush #:heat 1))
(define baby-empty-bowl (bowl #:color baby-brush #:heat 0 #:empty? #t))

(define papa-bear (bear (head #:hair papa-hair) papa-scale))
(define mama-bear (bear (head #:hair mama-hair) mama-scale))
(define baby-bear (bear (head #:hair baby-hair) baby-scale))
(define happy-baby-bear (bear (head #:hair baby-hair #:mouth 'grin) baby-scale))

(define (add-bowl p bear bear-scale bowl spoon?)
  (let* ([bowl (scale bowl bear-scale)]
         [bowl (if spoon?
                   (refocus (hb-append spoon bowl)
                            bowl)
                   bowl)])
    (pin-over p
              bear (shifted cc-find (- (/ (pict-width bowl) 2)) 0)
              bowl)))

(define (make-goldilocks+arms #:sit? [sit? #f]
                              #:eyes [eyes 'normal]
                              #:mouth [mouth 'normal]
                              #:show-arm [show-arm values]
                              #:elbow? [elbow? #f]
                              #:crash? [crash? #f]
                              #:crash-n [crash-n 1]
                              #:spoon? [spoon? #f])
  (define left-arm ((if spoon? add-spoon values)
                    (girl-arm #:rotate (* pi (+ -1/8 (if (or spoon? crash?) (* crash-n -1/2) 0))) #:bend? elbow?)))
  (define right-arm (girl-arm #:rotate (* pi (+ 1/8 (if (and elbow? (not crash?)) -1/4 (if crash? (* (- 1 crash-n) -1/4) 0)))) #:bend? elbow?))
  (define left-leg
    (if sit?
        (let ([d (pict-width girl-leg)])
          (inset (rotate girl-leg (* -1/4 pi)) (- (* 2.5 d)) 0 d 0))
        girl-leg))
  (define right-leg
    (if crash?
        (let ([d (* crash-n (pict-width girl-leg))]
              [xd (* (- 1 crash-n) (pict-width girl-leg))])
          (inset (rotate girl-leg (+ (* crash-n 1/4 pi)
                                     (* (- 1 crash-n) -1/4 pi)))
                 (+ (- (* 2 d)) (- (* 2.5 xd))) 0 (+ d xd) 0))
        left-leg))
  (values
   (body (inset (head #:bear? #f #:hair 'swoop #:hair-brush gold-brush
                      #:eyes eyes #:mouth mouth)
                0 0 0 20)
         (dress #:bend (if sit? 40 0))
         (show-arm left-arm)
         (show-arm right-arm)
         #:arm-shift 25 #:arm-squeeze 5
         left-leg right-leg)
   left-arm
   right-arm))

(define (make-goldilocks #:sit? [sit? #f]
                         #:eyes [eyes 'normal]
                         #:mouth [mouth 'normal]
                         #:spoon? [spoon? #f])
  (define-values (goldilocks left-arm right-arm)
    (make-goldilocks+arms #:sit? sit? #:eyes eyes #:mouth mouth #:spoon? spoon?))
  goldilocks)

(define goldilocks (make-goldilocks))

(define (combine-bears papa-bear mama-bear baby-bear)
  (hc-append (* 3 gap-size)
             papa-bear
             mama-bear
             baby-bear))

(define (table-scene #:bear-show [bear-show values]
                     #:papa-bear [papa-bear papa-bear]
                     #:mama-bear [mama-bear mama-bear]
                     #:baby-bear [baby-bear baby-bear]
                     #:papa-bowl [papa-bowl hot-papa-bowl]
                     #:mama-bowl [mama-bowl hot-mama-bowl]
                     #:baby-bowl [baby-bowl hot-baby-bowl]
                     #:spoons? [spoons? #t]
                     #:papa-spoon? [papa-spoon? spoons?]
                     #:mama-spoon? [mama-spoon? spoons?]
                     #:baby-spoon? [baby-spoon? spoons?]
                     #:away-n [away-n 0])
  (define g-papa-bear (ghost (launder papa-bear)))
  (define g-mama-bear (ghost (launder mama-bear)))
  (define g-baby-bear (ghost (launder baby-bear)))
  (define (add-bear p g-b b)
    (pin-under p
               g-b (shifted lt-find
                            (* away-n -1024)
                            0)
                            
               (bear-show b)))
  (let* ([p (vc-append (combine-bears g-papa-bear
                                      g-mama-bear
                                      g-baby-bear)
                       (scale table 4 2))]
         [p (add-bowl p g-papa-bear papa-scale papa-bowl papa-spoon?)]
         [p (add-bowl p g-mama-bear mama-scale mama-bowl mama-spoon?)]
         [p (add-bowl p g-baby-bear baby-scale baby-bowl baby-spoon?)]
         [p (add-bear p g-papa-bear papa-bear)]
         [p (add-bear p g-mama-bear mama-bear)]
         [p (add-bear p g-baby-bear baby-bear)])
    p))

(define (table-scene+goldilocks at-bear
                                #:spoon-bear [spoon-bear at-bear]
                                #:eyes [eyes 'normal]
                                #:mouth [mouth 'normal]
                                #:empty-bowl? [empty-bowl? #f]
                                #:away-n [away-n 0]
                                #:from-bear [from-bear #f]
                                #:from-n [from-n 0]
                                #:goldilocks [use-goldilocks #f])
  (let ([p (table-scene #:bear-show ghost
                        #:mama-bowl mama-cold-bowl
                        #:baby-bowl (if empty-bowl?
                                        baby-empty-bowl
                                        baby-just-right-bowl)
                        #:papa-spoon? (not (eq? spoon-bear papa-bear))
                        #:mama-spoon? (not (eq? spoon-bear mama-bear))
                        #:baby-spoon? (not (eq? spoon-bear baby-bear))
                        #:away-n away-n)])
    (define bear-delta (if from-bear
                           (let-values ([(fx fy) (lt-find p from-bear)]
                                        [(tx ty) (lt-find p at-bear)])
                             (- fx tx))
                           0))
    (if at-bear
        (pin-under p
                   at-bear (shifted lt-find
                                    (+ (/ (- (pict-width at-bear) (pict-width goldilocks)) 2)
                                       (* (- 1 from-n) bear-delta))
                                    (/ (- (pict-height at-bear) (pict-height goldilocks)) 2))
                   (or use-goldilocks
                       (make-goldilocks #:eyes eyes #:mouth mouth #:spoon? spoon-bear)))
        p)))

(define (table-slides)
  (define table-slide-name "Table")
  (define (table-slide s)
    (slide #:name table-slide-name s))
  (table-slide
   (table-scene))
  (table-slide
   (table-scene #:spoons? #f
                #:papa-bear (bear (head #:hair papa-hair #:mouth 'hot #:eyes 'wide) #:spoon? #t papa-scale)
                #:mama-bear (bear (head #:hair mama-hair #:mouth 'hot #:eyes 'wide) #:spoon? #t mama-scale)
                #:baby-bear (bear (head #:hair baby-hair #:mouth 'hot #:eyes 'wide) #:spoon? #t baby-scale)))
  
  (play-n
   #:name table-slide-name
   (lambda (n)
     (table-scene #:away-n (fast-end n))))
  (table-slide
   (table-scene #:bear-show ghost
                #:mama-bowl mama-cold-bowl
                #:baby-bowl baby-just-right-bowl))
  (play-n
   #:name table-slide-name
   #:skip-first? #t
   #:skip-last? #t
   (lambda (n)
     (table-scene+goldilocks papa-bear #:away-n (- 1 (fast-start n)) #:spoon-bear #f)))
  
  (table-slide
   (table-scene+goldilocks papa-bear #:spoon-bear #f))
  (table-slide
   (table-scene+goldilocks papa-bear #:eyes 'wide #:mouth 'hot))
  (play-n
   #:name table-slide-name
   #:skip-first? #t
   (lambda (n)
     (let ([n (fast-middle n)])
       (table-scene+goldilocks mama-bear #:from-bear papa-bear #:from-n n #:spoon-bear #f))))
  (table-slide
   (table-scene+goldilocks mama-bear #:eyes 'normal #:mouth 'frown))
  (play-n
   #:name table-slide-name
   #:skip-first? #t
   (lambda (n)
     (let ([n (fast-middle n)])
       (table-scene+goldilocks baby-bear #:from-bear mama-bear #:from-n n #:spoon-bear #f))))
  (table-slide
   (table-scene+goldilocks baby-bear #:mouth 'grin))
  (table-slide
   (table-scene+goldilocks baby-bear #:mouth 'grin #:empty-bowl? #t)))

(define (table-leave-scene n goldilocks)
  (table-scene+goldilocks baby-bear #:spoon-bear #f #:empty-bowl? #t #:away-n (- n)
                          #:goldilocks goldilocks))

;; --------------------------------------------------------------------------------

(define (change-rooms #:name name
                      #:skip-last? [skip-last? #f]
                      #:key [key-char goldilocks]
                      from-scene
                      to-scene)
  (define from-p (cc-superimpose titleless-page (from-scene 0 key-char)))
  (define to-p (cc-superimpose titleless-page (to-scene 1 key-char)))
  
  (define-values (from-x from-y) (lt-find from-p key-char))
  (define-values (to-x to-y) (lt-find to-p key-char))
  
  (define v-delta (* 0.5 (- to-y from-y)))
  (define h-delta (* 0.5 (- from-x to-x)))
  
  (play-n
   #:name name
   #:skip-first? #t
   #:skip-last? #t
   (lambda (n)
     (let ([n (fast-end n)])
       (if (= n 0)
           from-p
           (inset (from-scene n key-char)
                  (* n (- -1024 h-delta))
                  (* n v-delta)
                  (* n (+ 1024 h-delta))
                  (* n (- v-delta)))))))
  
  (play-n
   #:name name
   #:skip-first? #t
   #:skip-last? skip-last?
   (lambda (n)
     (let ([n (fast-middle n)])
       (if (= n 1)
           to-p
           (inset (to-scene n key-char)
                  (* (- 1 n) (+ 1024 h-delta))
                  (* (- 1 n) (- v-delta))
                  (* (- 1 n) (- -1024 h-delta))
                  (* (- 1 n) v-delta)))))))

;; --------------------------------------------------------------------------------

(define seat
  (let ([seat (new dc-path%)])
    (send seat move-to 75 240)
    (send seat line-to 200 240)
    (send seat curve-to 275 240 275 240 275 260)
    (send seat curve-to 275 265 275 265 265 280)
    (send seat line-to 200 320)
    (send seat line-to 170 330)
    (send seat line-to -20 330)
    (send seat curve-to -30 330 -40 330 -30 310)
    (send seat line-to 50 250)
    (send seat close)
    seat))

(define bottom
  (let ([seat (new dc-path%)])
    (send seat move-to 75 240)
    (send seat line-to 200 240)
    (send seat curve-to 275 240 275 240 275 360)
    (send seat curve-to 275 365 275 365 265 380)
    (send seat line-to 200 420)
    (send seat line-to 170 430)
    (send seat line-to -20 430)
    (send seat curve-to -30 430 -40 430 -30 410)
    (send seat line-to -30 320)
    (send seat close)
    seat))

(define (make-gradient base-brush)
  (define c (send base-brush get-color))
  (make-brush #:gradient
              (new linear-gradient%
                   [x0 0]
                   [y0 0]
                   [x1 300]
                   [y1 300]
                   [stops (list (list 0.0 c)
                                (list 1.0 (darker-color c 0.75)))])))

(define papa-chair
  (let ([chair-brush (make-gradient papa-brush)])
    (dc (lambda (dc x y)
          (with-state dc
            x y
            (send dc set-pen fat-black-pen)
            (send dc draw-line 120 300 120 400)
            (send dc draw-line 120 400 240 400)
            (send dc draw-line 120 400 0 400)
            (send dc draw-line 120 400 190 350)
            (send dc draw-line 120 400 50 450)
            (send dc draw-line 160 210 160 240)
            (send dc set-pen black-pen)
            (send dc set-brush chair-brush)
            (send dc draw-rounded-rectangle 50 0 225 225 20)
            (send dc draw-path seat 0 0)
            (void)))
        300 400)))

(define mama-chair
  (let ([chair-brush (make-gradient mama-brush)])
    (dc (lambda (dc x y)
          (with-state dc
            x y
            (send dc set-pen dark-gray-pen)
            (send dc set-brush chair-brush)
            (send dc draw-rounded-rectangle 50 30 225 225 20)
            (send dc draw-path bottom 0 10)
            (send dc draw-path seat 0 10)
            (void)))
        300 400)))

(define (make-baby-chair #:broken? broken?)
  (let ([chair-brush (make-gradient baby-brush)])
    (inset (scale
            (dc (lambda (dc x y)
                  (with-state dc
                    x y
                    (when broken?
                      (send dc translate 0 50))
                    (send dc set-pen fat-brown-pen)
                    (cond
                     [broken?
                      (send dc draw-line 70 270 -50 270)
                      (send dc draw-line 260 270 380 270)
                      (send dc draw-line 170 320 270 340)
                      (send dc draw-line -20 320 -120 340)]
                     [else
                      (send dc draw-line 70 320 70 360)
                      (send dc draw-line 260 270 260 360)
                      (send dc draw-line 170 320 170 410)
                      (send dc draw-line -20 320 -20 410)])
                    (send dc set-pen black-pen)
                    (send dc set-brush chair-brush)
                    (send dc draw-rounded-rectangle 50 0 225 225 20)
                    (send dc draw-path seat 0 -15)
                    (void)))
                300 400)
            0.8)
           0 0 0 -20)))

(define baby-chair (make-baby-chair #:broken? #f))
(define baby-broken-chair (make-baby-chair #:broken? #t))

(define tablet
  (colorize
   (rotate (scale (rotate (filled-rounded-rectangle 60 80 5) (* -1/4 pi)) 1 1.2)
           (* 1/4 pi))
   (make-color 100 100 100)))

(define monitor
  (vc-append
   (cb-superimpose (inset (lt-superimpose
                           (colorize (filled-rounded-rectangle 150 75 10)
                                     "gray")
                           (colorize (rounded-rectangle 150 75 10)
                                     "darkgray"))
                          0 0 0 10)
                   (colorize (filled-rectangle 10 60) "darkgray"))
   (colorize (filled-rectangle 40 5) "darkgray")))

(define (book title height)
  (define l (blank 0))
  (define r (blank 0))
  (define c (blank 0))
  (define b (blank 0))
  (define page (let* ([p (blank 50 60)]
                      [p (rt-superimpose p l)]
                      [p (lt-superimpose p c)]
                      [p (lb-superimpose p r)]
                      [p (rb-superimpose p b)])
                 p))
  (define cover
    (rotate (scale (rotate (cc-superimpose page (if (pict? title)
                                                    title
                                                    (t title)))
                           (* 9/8 pi))
                   1 1.2)
            (* -1/4 pi)))
  (define-values (lx ly) (lt-find cover l))
  (define-values (rx ry) (lt-find cover r))
  (define-values (cx cy) (lt-find cover c))
  (define-values (bx by) (lt-find cover b))
  (scale (lt-superimpose (dc (lambda (dc x y)
                               (with-state dc
                                 x y
                                 (send dc set-pen no-pen)
                                 (send dc set-brush light-tan-brush)
                                 (send dc draw-polygon (list (cons lx ly)
                                                             (cons lx (+ ly height))
                                                             (cons cx (+ cy height))
                                                             (cons cx cy)))
                                 (send dc set-brush lightest-tan-brush)
                                 (send dc draw-polygon (list (cons rx ry)
                                                             (cons bx by)
                                                             (cons lx ly)
                                                             (cons cx cy)))
                                 (send dc draw-polygon (list (cons cx (+ cy height))
                                                             (cons cx cy)
                                                             (cons rx ry)
                                                             (cons rx (+ ry height))))
                                 (send dc set-pen brown-pen)
                                 (send dc draw-line lx ly lx (+ ly height))
                                 (send dc set-pen dark-gray-pen)
                                 (send dc draw-lines (list (cons rx ry)
                                                           (cons bx by)
                                                           (cons lx ly)
                                                           (cons cx cy)))
                                 (send dc draw-line cx cy cx (+ cy height))
                                 (send dc draw-line rx ry rx (+ ry height))
                                 (send dc draw-line lx (+ ly height) cx (+ cy height))
                                 (send dc draw-line rx (+ ry height) cx (+ cy height))))
                             (pict-width cover)
                             (pict-height cover))
                         cover)
         1 0.5))

(define papa-book (book "++" 40))
(define mama-book (book "\u3BB" 5))
(define baby-book-height 20)
(define baby-book (book "\u2605" baby-book-height))

(define (desk height)
  (dc (lambda (dc x y)
        (with-state dc
          x y
          (send dc set-pen no-pen)
          (send dc set-brush brown-brush)
          (send dc set-pen dark-gray-pen)
          (send dc translate -40 0)
          (send dc draw-polygon (list (cons 0 75)
                                      (cons 250 75)
                                      (cons 250 (+ height 50 75))
                                      (cons 0 (+ height 50 75))))
          (send dc draw-polygon (list (cons 350 0)
                                      (cons 250 75)
                                      (cons 250 (+ height 50 75))
                                      (cons 350 (+ height 50 0))))
          (send dc set-brush tan-brush)
          (send dc draw-polygon (list (cons 0 75)
                                      (cons 100 0)
                                      (cons 350 0)
                                      (cons 250 75)))))
      350 (+ 75 height)))

(define papa-desk (desk 80))
(define mama-desk (desk 70))
(define baby-desk (desk 60))

(define (sit-scene #:on [on #f]
                   #:sit? [sit? #t]
                   #:desk? [desk? #f]
                   #:tablet? [tablet? (not desk?)]
                   #:floor-tablet? [floor-tablet? tablet?]
                   #:eyes [eyes 'normal]
                   #:mouth [mouth 'normal]
                   #:tablet-chair [tablet-chair on]
                   #:crash? [crash? #f]
                   #:crash-n [crash-n 1]
                   #:papa-chair [papa-chair papa-chair]
                   #:mama-chair [mama-chair mama-chair]
                   #:baby-chair [baby-chair baby-chair]
                   #:papa-desk? [papa-desk? desk?]
                   #:mama-desk? [mama-desk? desk?]
                   #:baby-desk? [baby-desk? desk?]
                   #:just-baby? [just-baby? #f]
                   #:baby-book [baby-book baby-book]
                   #:away-n [away-n 0]
                   #:hop-n [hop-n 0]
                   #:from-chair [from-chair #f]
                   #:from-n [from-n 0]
                   #:goldilocks [goldilocks #f])
  (define chair-sep (* 2 gap-size))
  (define chair-scale 0.75)
  (define (sit-on chair desk desk? #:dy [dy 0] #:desk-dx [desk-dx 0])
    (define sc-chair (let ([p (scale chair chair-scale)])
                       (if (or (eq? chair tablet-chair)
                               (not floor-tablet?)
                               desk?)
                           p
                           (refocus (hbl-append -120
                                                p
                                                (rotate tablet (* pi 1/2)))
                                    p))))
    (define d-desk (inset (ghost desk)
                          (- desk-dx) 0 desk-dx 0))
    (refocus (if (eq? chair on)
                 (let-values ([(goldilocks left-arm right-arm)
                               (if sit?
                                   (make-goldilocks+arms #:sit? #t #:elbow? tablet-chair #:crash? crash? #:crash-n crash-n
                                                         #:eyes eyes #:mouth mouth)
                                   (if goldilocks
                                       (values goldilocks #f #f)
                                       (make-goldilocks+arms #:eyes eyes #:mouth mouth)))])
                   (let* ([p (cb-superimpose
                              sc-chair
                              (cb-superimpose
                               (if (and crash? sit?)
                                   (inset (rotate goldilocks (* crash-n -1/2 pi)) 0 (- dy) 0 dy)
                                   (let ([dy (+ (if sit? dy (- dy 100))
                                                (* hop-n 100))]
                                         [dx (- (* away-n -1024)
                                                (if from-chair
                                                    (let ([delta (+ (* chair-scale
                                                                       (/ (+ (pict-width chair) (pict-width from-chair)) 2))
                                                                    chair-sep)])
                                                      (* (- 1 from-n) delta))
                                                    0))])
                                     (inset goldilocks dx (- dy) (- dx) dy)))
                               (ghost (inset tablet
                                             0 0
                                             (* (if crash? (- 1 crash-n) 1) 120)
                                             (+ (* (if crash? (- 1 crash-n) 1) 90) dy))))
                              d-desk)]
                          [p (if (and desk? sit?)
                                 (pin-over p left-arm lt-find left-arm)
                                 p)]
                          [p (if (and desk? sit?)
                                 (pin-over p right-arm lt-find right-arm)
                                 p)]
                          [p (if (and tablet? tablet-chair)
                                 (pin-over p tablet lt-find tablet)
                                 p)])
                     p))
                 (cb-superimpose sc-chair d-desk))
             sc-chair))
  (define (add-monitor p desk? desk book)
    (if desk?
        (pin-over (pin-over (pin-over p
                                      desk lt-find
                                      desk)
                            desk lt-find
                            (inset book 150 -10))
                  desk lt-find
                  (inset monitor 30 -20))
        p))
  (define baby-scene
    (sit-on (cond
             [(eq? on baby-chair) baby-chair]
             [crash? baby-broken-chair]
             [else baby-chair])
            baby-desk baby-desk? #:dy -10))
  (if just-baby?
      (add-monitor baby-scene baby-desk? baby-desk baby-book)
      (let* ([p (hb-append chair-sep
                           (sit-on papa-chair papa-desk papa-desk? #:dy 10)
                           (sit-on mama-chair mama-desk mama-desk? #:desk-dx 10)
                           baby-scene)]
             [p (add-monitor p papa-desk? papa-desk papa-book)]
             [p (add-monitor p mama-desk? mama-desk mama-book)]
             [p (add-monitor p baby-desk? baby-desk baby-book)])
        p)))

(define (desk-scene #:on [on #f]
                    #:sit? [sit? #t]
                    #:eyes [eyes 'normal]
                    #:mouth [mouth 'normal]
                    #:just-baby? [just-baby? #f]
                    #:baby-book [baby-book baby-book]
                    #:papa-chair [papa-chair papa-chair]
                    #:mama-chair [mama-chair mama-chair]
                    #:papa-desk? [papa-desk? #t]
                    #:mama-desk? [mama-desk? #t]
                    #:away-n [away-n 0]
                    #:hop-n [hop-n 0]
                    #:from-chair [from-chair #f]
                    #:from-n [from-n 0]
                    #:goldilocks [use-goldilocks #f]
                    ;; For `change-chair`:
                    #:tablet-chair [table-chair #f])
  (sit-scene #:desk? #t #:on on #:eyes eyes #:mouth mouth #:just-baby? just-baby?
             #:baby-book baby-book
             #:papa-chair papa-chair
             #:mama-chair mama-chair
             #:papa-desk? papa-desk?
             #:mama-desk? mama-desk?
             #:sit? sit?
             #:away-n away-n
             #:hop-n hop-n
             #:from-chair from-chair
             #:from-n from-n
             #:goldilocks use-goldilocks))

(define (hop-chair chair #:on? on? #:name name)
  (play-n
   #:name name
   #:skip-first? #t
   #:skip-last? #t
   (lambda (n)
     (let ([n (if on? n (- 1 n))])
       (define sit? ((* n 3/4) . > . 1/2))
       (sit-scene #:sit? sit?
                  #:on chair #:tablet-chair #f #:hop-n (- (/ (sin (* n 3/4 pi)) (sin (* 3/4 pi)))
                                                          (if sit? 1 0))
                  #:goldilocks goldilocks)))))

(define (change-chair #:from from-chair #:to to-chair #:name name
                      #:scene [scene sit-scene] #:hop-n [hop-n 0])
  (play-n
   #:name name
   #:skip-first? #t
   #:skip-last? #t
   (lambda (n)
     (scene #:sit? #f #:on to-chair #:from-chair from-chair #:from-n (fast-middle n)
            #:tablet-chair #f
            #:hop-n hop-n
            #:goldilocks goldilocks))))

(define (chair-slides)
  (define chair-name "Chairs")
  (define (sit-slide s)
    (slide #:name chair-name s))
  
  (change-rooms
   #:name chair-name
   #:skip-last? #t
   table-leave-scene
   (lambda (n goldilocks)
     (sit-scene #:sit? #f #:on papa-chair #:tablet-chair #f #:away-n (- 1 n)
                #:goldilocks goldilocks)))
  
  (hop-chair papa-chair #:on? #t #:name chair-name)
  (sit-slide
   (sit-scene #:on papa-chair #:tablet-chair #f))
  (sit-slide
   (sit-scene #:on papa-chair))
  (sit-slide
   (sit-scene #:on papa-chair #:eyes 'angry #:mouth 'frown))
  
  (hop-chair papa-chair #:on? #f #:name chair-name)
  (change-chair #:from papa-chair #:to mama-chair #:name chair-name)
  (hop-chair mama-chair #:on? #t #:name chair-name)
  
  (sit-slide
   (sit-scene #:on mama-chair #:tablet-chair #f))
  (sit-slide
   (sit-scene #:on mama-chair))
  (sit-slide
   (sit-scene #:on mama-chair #:eyes 'tired #:mouth 'straight))
  
  (hop-chair mama-chair #:on? #f #:name chair-name)
  (change-chair #:from mama-chair #:to baby-chair #:name chair-name)
  (hop-chair baby-chair #:on? #t #:name chair-name)
  
  (sit-slide
   (sit-scene #:on baby-chair #:tablet-chair #f))
  (sit-slide
   (sit-scene #:on baby-chair))
  (sit-slide
   (sit-scene #:on baby-chair #:eyes 'wide #:mouth 'grin))
  (sit-slide
   (sit-scene #:on baby-chair #:eyes 'cry #:mouth 'big-grin))
  (play-n
   #:name chair-name
   #:skip-first? #t
   (lambda (n)
     (let ([n (fast-middle n)])
       (define mouth (if (n . > . 0.5) 'hot 'big-grin))
       (define chair (if (n . > . 0.5) baby-broken-chair baby-chair))
       (sit-scene #:on chair #:eyes 'cry #:mouth mouth #:crash? #t #:crash-n n))))
  
  (play-n
   #:name chair-name
   #:skip-first? #t
   #:skip-last? #t
   (lambda (n)
     (let ([n (fast-middle (- 1 n))])
       (define sit? (n . > . 0.5))
       (sit-scene #:sit? sit? #:on baby-broken-chair #:eyes 'worried #:crash? #t #:crash-n n #:tablet-chair #f))))
  
  (sit-slide
   (chair-leave-scene 0 #f)))

(define (chair-leave-scene n goldilocks)
   (sit-scene #:sit? #f #:on baby-broken-chair #:eyes 'worried #:crash? #t #:tablet-chair #f
              #:goldilocks goldilocks #:away-n (- n)))

(define (desk-slides)
  (define desk-name "Desks")
  
  (define (desk-slide s)
    (slide #:name desk-name s))
  
  (change-rooms
   #:name desk-name
   #:skip-last? #t
   chair-leave-scene
   (lambda (n goldilocks)
     (desk-scene #:sit? #f #:on papa-chair #:away-n (- 1 n) #:hop-n 1
                 #:goldilocks goldilocks)))
  
  (desk-slide
   (desk-scene #:on papa-chair))
  (desk-slide
   (desk-scene #:on papa-chair #:eyes 'dazed #:mouth 'straight))
  (change-chair #:scene desk-scene #:from papa-chair #:to mama-chair #:name desk-name #:hop-n 1)
  (desk-slide
   (desk-scene #:on mama-chair))
  (desk-slide
   (desk-scene #:on mama-chair #:eyes 'rolled #:mouth 'straight))
  (change-chair #:scene desk-scene #:from mama-chair #:to baby-chair #:name desk-name #:hop-n 1)
  (desk-slide
   (desk-scene #:on baby-chair))
  (desk-slide
   (desk-scene #:on baby-chair #:eyes 'kinda-wide #:mouth 'grin)))

;; --------------------------------------------------------------------------------

(define outraged-bears
  (combine-bears
   (bear (head #:hair papa-hair #:mouth 'hot #:eyes 'wide) papa-scale)
   (bear (head #:hair mama-hair #:mouth 'hot #:eyes 'wide) mama-scale)
   (bear (head #:hair baby-hair #:mouth 'hot #:eyes 'wide) baby-scale)))

(define (outrage-scene room
                       #:bears [bears outraged-bears]
                       #:away-n [away-n 0])
  (cc-superimpose
   (lt-superimpose
    full-page
    (inset bears
           (* away-n 1024) 0 (* away-n -1024) 0))
   room))

(define (baby-bear-spectator p #:move-n [n 1])
  (define-values (ex ey) ((shifted lt-find -50 -100) p baby-chair))
  (define-values (sx sy) (if (= n 1)
                             (values ex ey)
                             (lt-find p baby-bear)))
  (define (along a b) (+ (* (- 1 n) a) (* n b)))
  (pin-under p
             (along sx ex) (along sy ey)
             happy-baby-bear))

(define (end-slides)
  (define end-name "Bears Return")
  (define (end-slide s)
    (slide #:name end-name s))
  
  (end-slide
   (outrage-scene (table-scene+goldilocks #f #:empty-bowl? #t)))
  
  (change-rooms
   #:name end-name
   #:key outraged-bears
   (lambda (n bears)
     (outrage-scene (table-scene+goldilocks #f #:empty-bowl? #t) #:away-n n))
   (lambda (n bears)
     (outrage-scene (sit-scene #:on #f #:crash? #t) #:away-n (- n 1))))
  
  (change-rooms
   #:name end-name
   #:key outraged-bears
   (lambda (n bears)
     (outrage-scene (sit-scene #:on #f #:crash? #t) #:away-n n))
   (lambda (n bears)
     (outrage-scene (desk-scene #:on baby-chair #:eyes 'kinda-wide #:mouth 'grin) #:away-n (- n 1))))
  
  (play-n
   #:skip-first? #t
   #:name end-name
   (lambda (n)
     (baby-bear-spectator
      #:move-n (fast-middle n)
      (outrage-scene (desk-scene #:on baby-chair #:eyes 'kinda-wide #:mouth 'grin)
                     #:bears (combine-bears papa-bear
                                            mama-bear
                                            (ghost baby-bear))))))
  (slide
   #:name "The End"
   (lc-superimpose
    (baby-bear-spectator
     (desk-scene #:on baby-chair #:eyes 'kinda-wide #:mouth 'grin
                 #:mama-chair (ghost mama-chair)
                 #:papa-chair (ghost papa-chair)
                 #:papa-desk? #f
                 #:mama-desk? #f))
    (inset (let ([et (lambda (s) (text s '(italic . "Zapfino") 48))])
             (vc-append (* -2 gap-size)
                        (et "The")
                        (inset (et "End") gap-size 0 0 0)))
           (* client-w 1/6)))))

;; --------------------------------------------------------------------------------

(define (once p pad)
  (define bm (make-bitmap (inexact->exact (round (+ (pict-width p) (* 2 pad))))
                          (inexact->exact (round (+ (pict-height p) (* 2 pad))))
                          #:backing-scale 2))
  (define bm-dc (send bm make-dc))
  (send bm-dc set-smoothing 'aligned)
  (draw-pict p bm-dc pad pad)
  (dc (lambda (dc x y)
        (send dc draw-bitmap bm (- x pad) (- y pad)))
      (pict-width p)
      (pict-height p)))

(define papa-chef-bear
  (bear (head #:hair 'chef)
        papa-scale))

(define (impossible-slides)
  (define (impossible-slide s)
    (slide #:name "Impossibles" s))
  (impossible-slide
   (table-scene #:papa-bear papa-chef-bear
                #:mama-bear (ghost mama-bear)
                #:baby-bear (ghost baby-bear)))
  (impossible-slide
   (sit-scene #:on mama-chair))
  (impossible-slide
   (baby-bear-spectator
    (desk-scene #:on baby-chair #:eyes 'kinda-wide #:mouth 'grin)))
  
  (impossible-slide
   (let ([gen (make-pseudo-random-generator)])
     (parameterize ([current-pseudo-random-generator gen])
       (random-seed 425))
     (define X 20)
     (define (adj v n) (* (/ v X) n))
     (cc-superimpose
      (cellophane-pane
       #:margin margin
       (for/fold ([p full-page]) ([i (in-range 70)])
         (let ([p (if (= i 60)
                      (cellophane-pane #:margin margin p 0.5)
                      p)])
           (pin-over p (- (adj (random X gen) client-w) 50)
                     (- (adj (random X gen) client-h) 50)
                     papa-chef-bear)))
       0.5)
      papa-chef-bear)))
  
  (impossible-slide
   (cc-superimpose
    (cellophane
     (apply vc-append
            (for/list ([i (in-range 6)])
              (apply hc-append
                     (for/list ([i (in-range 9)])
                       (inset tablet (- gap-size))))))
     0.5)
    (sit-scene #:on mama-chair
               #:floor-tablet? #f
               #:papa-chair (ghost papa-chair)
               #:baby-chair (ghost baby-chair))))
    
  (impossible-slide
   (lt-superimpose
    (cellophane-pane
     (let ([fail (inset (scale (desk-scene #:just-baby? #t #:on baby-chair #:eyes 'normal #:mouth 'straight)
                               0.5)
                        70 40 30 40)])
       (apply vc-append
              (for/list ([j 4])
                (apply hc-append
                       (for/list ([i 5])
                         (if (and (<= 0 j 1)
                                  (<= 0 i 1))
                             (ghost fail)
                             fail))))))
     0.5)
    (inset
     (baby-bear-spectator
      (desk-scene #:just-baby? #t #:on baby-chair #:eyes 'kinda-wide #:mouth 'grin))
     140 100 -140 -100))))

;; --------------------------------------------------------------------------------

(define (test-slides)
  (slide
   (hc-append
    (* 4 gap-size)
    (make-goldilocks #:mouth 'grin
                     #:eyes 'cry)
    (bear (head #:hair 'spiky
                #:mouth 'hot
                #:eyes 'wide)
          papa-scale))))
 

(define (goldilocks-slides)
  (table-slides)
  (chair-slides)
  (desk-slides)
  (end-slides)
  (impossible-slides))

(define (many-desks-slides logo)
  (define books '("\u262F"
                  "\u2766"
                  "\u2749"
                  "\u273F"
                  "\u2764"))
  (define (shift p) (inset p (* 2 gap-size) 0 0 0))
  (slide
   (shift
    (baby-bear-spectator
     (desk-scene #:just-baby? #t #:on baby-chair #:eyes 'kinda-wide #:mouth 'grin
                 #:baby-book (book logo baby-book-height)))))
  (slide
   (blank gap-size)
   (scale
    (apply vc-append
           (* 8 gap-size)
           (for/list ([j 2])
             (apply hc-append
                    (* 8 gap-size)
                    (for/list ([i 3])
                      (shift
                       (baby-bear-spectator
                        (desk-scene #:just-baby? #t #:on baby-chair #:eyes 'kinda-wide #:mouth 'grin
                                    #:baby-book (if (= i j 0)
                                                    baby-book
                                                    (book (list-ref books (+ i (* j 3) -1))
                                                          baby-book-height)))))))))
    0.75)))

(module+ main
  (require "logo.rkt")
  (goldilocks-slides)
  (many-desks-slides (scale racket-logo 0.1)))
