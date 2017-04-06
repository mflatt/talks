#lang slideshow
(require scheme/class
         scheme/gui/base)

(provide make-princess)

(define (make-princess #:side [side 'front]
                       #:rotate [angle 0.0]
                       #:clip-body [clip-body #f]
                       #:clip-dx [clip-dx 0]
                       #:clip-dy [clip-dy 0]
                       #:dress [dress-color "pink"]
                       #:hair [hair-color "yellow"]
                       #:pen-width [pen-width 1]
                       #:arm-angle [arm-angle 0.0]
                       #:leg-angle [leg-angle 0.0]
                       #:front-arm-angle [front-arm-angle 0.0]
                       #:smile? [smile? #t]
                       #:shake [shake 0])
 (let* ([crown (new dc-path%)]
        [hair (new dc-path%)]
        [front-hair (new dc-path%)]
        [head (new dc-path%)]
        [front-head (new dc-path%)]
        [mouth (new dc-path%)]
        [front-mouth (new dc-path%)]
        [front-nose (new dc-path%)]
        [eye (new dc-path%)]
        [front-eye (new dc-path%)]
        [front-eye2 (new dc-path%)]
        [eyedot (new dc-path%)]
        [front-eyedot (new dc-path%)]
        [front-eyedot2 (new dc-path%)]
        [sleeve (new dc-path%)]
        [front-sleeve (new dc-path%)]
        [front-sleeve2 (new dc-path%)]
        [body (new dc-path%)]
        [dress (new dc-path%)]
        [arm (new dc-path%)]
        [right-arm (new dc-path%)]
        [left-arm (new dc-path%)]
        [foot (new dc-path%)]
        [foot2 (new dc-path%)]
        [front-foot (new dc-path%)]
        [front-foot2 (new dc-path%)]
        [leg (new dc-path%)]
        [leg2 (new dc-path%)]
        [front-leg (new dc-path%)]
        [front-leg2 (new dc-path%)]
        [as (abs shake)]
        [pen-color "black"]
        [pen-mode 'solid])
   (send crown move-to 0 0)
   (send crown line-to 0 -20)
   (send crown line-to (+ 5 (/ as 2)) -10)
   (send crown line-to (+ 10 (/ as 2)) -20)
   (send crown line-to 15 -10)
   (send crown line-to (- 20 (/ as 2)) -20)
   (send crown line-to (- 25 (/ as 2)) -10)
   (send crown line-to 30 -20)
   (send crown line-to 30 0)
   (send crown close)

   (let* ([make-hair
           (lambda (hair front?)
             (send hair move-to (+ 0 as) 30)
             (send hair curve-to (+ 3 as) 35 (+ 3 as) 35 (+ as 5) 35)
             (send hair curve-to (+ 10 (/ as 2)) 35 (+ 10 (/ as 2)) 0 20 0)
             (if front?
                 (begin
                   (send hair line-to 35 0)
                   (send hair curve-to (- 45 (/ as 2)) 0 (- 45 (/ as 2)) 35 (- 50 as) 35)
                   (send hair curve-to (- 52 as) 35 (- 52 as) 35 (- 55 as) 30)
                   (send hair line-to (- 55 as) 35)
                   (send hair curve-to (- 55 as) 40 (- 45 as) 40 (- 48 as) 40)

                   (send hair curve-to (- 43 (/ as 2)) 40 (+ 38 (min 0 shake)) 40 (+ 38 (min 0 shake)) 20)
                   (send hair curve-to (+ 38 (min 0 shake)) 10 (+ 38 (min 0 shake)) 10 (+ 27.5 (min 0 shake)) 10)
                   (send hair curve-to (+ 17 (max 0 shake)) 10 (+ 17 (max 0 shake)) 10 (+ 17 (max 0 shake)) 20)
                   (send hair curve-to (+ 17 (max 0 shake)) 40 12 40 7 40))
                 (begin
                   (send hair line-to 30 0)
                   (send hair curve-to 40 0 40 0 40 10)
                   (send hair curve-to 20 10 20 15 20 20)
                   (send hair curve-to 20 30 20 40 15 40)))
             (send hair curve-to (+ 10 (/ as 2)) 40 (+ 0 as) 40 (+ 0 as) 35)
             (send hair close))])
     (make-hair hair #f)
     (make-hair front-hair #t))

   (send front-hair translate 1 15)
                       
   (send head move-to 20 30)
   (send head curve-to 20 35 20 35 28 35)
   (send head curve-to 33 35 38 30 38 22)
   (send head line-to 42 21)
   (send head curve-to 38 20 38 10 38 5)
   (send head line-to 20 5)
   (send head close)

   (send front-head move-to 38 22)
   (send front-head curve-to 38 30 33 35 28 35)
   (send front-head line-to 27 35)
   (send front-head curve-to 22 35 17 30 17 22)
   (send front-head line-to 17 8)
   (send front-head line-to 38 8)
   (send front-head close)

   (send front-head translate 1 15)

   (send mouth move-to 37 27)
   (send mouth curve-to 34 25 34 28 30 24)
   
   (send front-mouth move-to 34 24)
   (if smile?
       (send front-mouth curve-to 28 28 27 28 21 24)
       (begin
         (send front-mouth curve-to 28 20 27 20 21 24)
         (send front-mouth translate 0 4)))
   (send front-mouth translate (+ 1 shake) 15)

   (send front-nose move-to 29 21)
   (send front-nose curve-to 28 24 27 24 26 21)
   (send front-nose translate (+ 1 shake) 15)
   
   (send eye ellipse 30 13 4 4)
   (send front-eye append eye)
   (send eyedot ellipse 32 14.5 1 1)
   (send front-eyedot ellipse 31.5 14.5 1 1)

   (send front-eye2 append front-eye)
   (send front-eyedot2 append front-eyedot)

   (send front-eye translate (- shake 1) 15)
   (send front-eye2 translate (- shake 7) 15)
   (send front-eyedot translate (- shake 1) 15)
   (send front-eyedot2 translate (- shake 7) 15)

   (send sleeve ellipse 5 33 20 15)

   (send front-sleeve ellipse 5 33 10 15)
   (send front-sleeve2 append front-sleeve)

   (send front-sleeve translate 9 15)
   (send front-sleeve2 translate 27 15)

   (send arm move-to 0 0)
   (send arm line-to 0 17)
   (send arm curve-to 0 21 0 23 3 23)
   (send arm curve-to 5 23 5 23 6 17)
   (send arm line-to 6 0)
   (send arm close)
   (send arm translate -3 0)

   (send right-arm append arm)
   (send left-arm append arm)

   (send right-arm rotate (+ (/ pi 10) front-arm-angle))
   (send left-arm rotate (- (/ pi -10) front-arm-angle))

   (send left-arm translate 18 62)
   (send right-arm translate 38 62)

   (unless (zero? arm-angle)
     (send arm rotate arm-angle))

   (send body rectangle 7 36 16 26)

   (send dress move-to 7 62)
   (send dress line-to -13 95)
   (send dress line-to 43 95)
   (send dress line-to 23 62)
   (send dress close)

   (send foot ellipse 10 110 15 5)
   (send leg rectangle 11 92 7 20)
   (send foot translate -14.5 -72)
   (send leg translate -14.5 -72)

   (send foot2 append foot)
   (send leg2 append leg)

   (send foot rotate leg-angle)
   (send leg rotate leg-angle)
   (send foot2 rotate (- leg-angle))
   (send leg2 rotate (- leg-angle))

   (send front-leg rectangle 11.5 92 7 20)
   (send front-foot ellipse 10 110 10 5)

   (send front-leg2 append front-leg)
   (send front-foot2 append front-foot)

   (send front-leg translate 7 15)
   (send front-foot translate 7 15)
   (send front-leg2 translate 19 15)
   (send front-foot2 translate 19 15)

   (send leg translate (+ 13 14.5) 87)
   (send leg2 translate (+ 13 14.5) 87)
   (send body translate 13 15)
   (send dress translate 13 15)
   (send foot translate (+ 13 14.5) 87)
   (send foot2 translate (+ 13 14.5) 87)
   (send arm translate (+ 13 15) 62)
   (send sleeve translate 13 15)
   (send head translate (- 13 12) 15)
   (send mouth translate (- 13 12) 15)
   (send eye translate (- 13 12) 15)
   (send eyedot translate (- 13 12) 15)
   (send hair translate (- 13 12) 15)
   (send crown translate 13 20)

   (when (or (eq? side 'left) (not (zero? angle)))
     (map (lambda (i)
            (when (eq? side 'left)
              (send i scale -1 1)
              (send i translate 56 0))
            (unless (zero? angle)
              (send i rotate angle)))
          (list leg
                leg2
                body
                dress
                foot
                foot2
                arm
                sleeve
                head
                mouth
                eye
                eyedot
                hair
                crown
                front-leg
                front-leg2
                front-foot
                front-foot2
                left-arm
                right-arm
                front-sleeve
                front-sleeve2
                front-hair
                front-head
                front-mouth
                front-nose
                front-eye
                front-eye2
                front-eyedot
                front-eyedot2)))

   (if (not (eq? side 'front))
       (dc (lambda (dc x y)
             (let-values ([(p) (send dc get-pen)]
                          [(b) (send dc get-brush)])
               (send dc set-pen pen-color pen-width pen-mode)
               (send dc set-brush "peachpuff" 'solid)
               (send dc draw-path leg2 x y)
               (send dc draw-path leg x y)
               (send dc set-brush dress-color 'solid)
               (send dc draw-path body x y)
               (send dc draw-path dress x y)
               (send dc draw-path foot2 x y)
               (send dc draw-path foot x y)
               (send dc set-brush "peachpuff" 'solid)
               (send dc draw-path arm x y)
               (send dc set-brush dress-color 'solid)
               (send dc draw-path sleeve x y)
               (send dc set-brush "peachpuff" 'solid)
               (send dc draw-path head x y)
               (send dc set-pen pen-color 1 'solid)
               (send dc draw-path mouth x y)
               (send dc set-brush "white" 'solid)
               (send dc draw-path eye x y)
               (send dc draw-path eyedot x y)
               (send dc set-pen pen-color pen-width pen-mode)
               (send dc set-brush hair-color 'solid)
               (send dc draw-path hair x y)
               (send dc set-brush dress-color 'solid)
               (send dc draw-path crown x y)
               (send dc set-pen p)
               (send dc set-brush b)))
           56 130)
       (dc (lambda (dc x y)
             (let ([p (send dc get-pen)]
                   [b (send dc get-brush)]
                   [c (and clip-body
                           (send dc get-clipping-region))])
               (send dc set-pen pen-color pen-width pen-mode)
               (send dc set-brush "peachpuff" 'solid)
               (when clip-body
                 (let ([c (make-object region% dc)])
                   (send c set-path clip-body (+ x clip-dx) (+ y clip-dy))
                   (send dc set-clipping-region c)))
               (send dc draw-path front-leg x y)
               (send dc draw-path front-leg2 x y)
               (send dc set-brush dress-color 'solid)
               (send dc draw-path body x y)
               (send dc draw-path dress x y)
               (send dc draw-path front-foot x y)
               (send dc draw-path front-foot2 x y)
               (send dc set-brush "peachpuff" 'solid)
               (send dc draw-path left-arm x y)
               (send dc draw-path right-arm x y)
               (send dc set-brush dress-color 'solid)
               (send dc draw-path front-sleeve x y)
               (send dc draw-path front-sleeve2 x y)
               (when clip-body
                 (send dc set-clipping-region c))
               (send dc set-brush "peachpuff" 'solid)
               (send dc draw-path front-head x y)
               (send dc set-pen pen-color 1 'solid)
               (send dc draw-path front-mouth x y)
               (send dc draw-path front-nose x y)
               (send dc set-brush "white" 'solid)
               (send dc draw-path front-eye x y)
               (send dc draw-path front-eye2 x y)
               (send dc draw-path front-eyedot x y)
               (send dc draw-path front-eyedot2 x y)
               (send dc set-pen pen-color pen-width pen-mode)
               (send dc set-brush hair-color 'solid)
               (send dc draw-path front-hair x y)
               (send dc set-brush dress-color 'solid)
               (send dc draw-path crown x y)
               (send dc set-pen p)
               (send dc set-brush b)))
           56 130))))
