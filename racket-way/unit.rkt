#lang slideshow
(require racket/gui/base
         racket/class)

(provide example-unit)

(define gen
  (lambda (second? link? cpd? propagate? mode2?)
    (let ([s 1])    ; scale
      (inset
       (dc (lambda (dc x y)
             (define orig-b (send dc get-brush))
             (define orig-p (send dc get-pen))
             (define t (send dc get-transformation))

             (define bluepen
               (send the-pen-list
                     find-or-create-pen
                     "blue"
                     2
                     'solid))
             (define orangepen
               (send the-pen-list
                     find-or-create-pen
                     "orange"
                     4
                     'solid))
             (define tt-font
               (send the-font-list
                     find-or-create-font
                     24
                     'modern
                     'normal
                     'bold))
             (define it-font
               (send the-font-list
                     find-or-create-font
                     24
                     'swiss
                     'italic
                     'normal))
             
             (define (draw-unit draw? x y name import body-l export)
               (let-values ([(nw nh nd na)
                             (send dc get-text-extent name it-font)]
                            [(iw ih id ia)
                             (send dc get-text-extent import tt-font)]
                            [(ew eh ed ea)
                             (if export
                                 (send dc get-text-extent export tt-font)
                                 (values 0 0 0 0))]
                            [(bw bh)
                             (if (number? (car body-l))
                                 (values (car body-l) (cadr body-l))
                                 (let loop ([w 0][h 0][l body-l])
                                   (if (null? l)
                                       (values w h)
                                       (let-values ([(1w 1h 1d 1a)
                                                     (send dc get-text-extent (car l) tt-font)])
                                         (loop (max w 1w) (+ h 1h) (cdr l))))))])
                 (let ([w (+ (max iw ew bw) 10)]
                       [h (+ ih eh bh)])
                   (when draw?
                     (send dc draw-rectangle x (+ y nh) w h)
                     (send dc draw-line x (+ y nh ih) (+ x w -1) (+ y nh ih))
                     (send dc draw-line x (+ y nh ih bh) (+ x w -1) (+ y nh ih bh))

                     (send dc set-font it-font)
                     
                     (send dc draw-text name x y)
                     
                     (send dc set-font tt-font)
                     
                     (send dc draw-text import (+ x (/ (- w iw) 2)) (+ y nh))
                     (send dc draw-text export (+ x (/ (- w ew) 2)) (+ y nh ih bh))

                     (unless (number? (car body-l))
                       (let loop ([l body-l][y (+ y nh ih)])
                         (unless (null? l)
                           (send dc draw-text (car l) (+ x 5) y)
                           (let-values ([(1w 1h 1d 1a) (send dc get-text-extent (car l))])
                             (loop (cdr l) (+ y 1h)))))))

                   (values w (+ nh h) nh ih eh))))

             (define (connect x1 y1 x2 y2)
               (let ([xm (/ (+ x1 x2) 2)]
                     [ym (/ (+ y1 y2) 2)])
                 (send dc draw-spline 
                       x1 y1
                       (/ (+ x1 xm) 2) (- y1 50)
                       xm ym)
                 (send dc draw-spline 
                       x2 y2
                       (/ (+ x2 xm) 2) (+ y2 50)
                       xm ym)

                 (send dc draw-line
                       x2 y2
                       (+ x2 (if (> x1 x2) 15 -15)) y2)
                 (send dc draw-line
                       x2 y2
                       x2 (+ y2 15))))

             (define (up-connect x1 y1 x2 y2)
               (send dc draw-line x1 y1 x2 y2)

               (send dc draw-line x2 y2 (- x2 10) (+ y2 10))
               (send dc draw-line x2 y2 (+ x2 10) (+ y2 10)))

             (send dc translate x y)
             (send dc scale s s)
             
             (send dc set-pen bluepen)
             
             (if mode2?
                 ;; Completely different layout
                 (let*-values ([(ex ey) (values 55 15)]
                               [(ew eh en ei ee)
                                (draw-unit #t ex ey
                                           "Interp"
                                           ""
                                           '(420 300)
                                           "evaluate")]
                               [(dx dy) (values (+ ex 10) (+ ey en ei 5))]
                               [(ds) "(define (evaluate s) ...)"]
                               [(dw dh da dd)
                                (send dc get-text-extent ds tt-font)]
                               [(is) "     (invoke "]
                               [(iw ih ia id)
                                (send dc get-text-extent is tt-font)]
                               [(hx hy) (values (+ ex 10 iw 10) (+ dy (* 3 dh)))]
                               [(hw hh hn hi he)
                                (draw-unit #t hx hy
                                           "Help"
                                           "evaluate"
                                           '(""
                                             "..."
                                             "")
                                           "find-help")])
                   (send dc set-font tt-font)
                   (send dc draw-text ds dx dy)
                   (send dc draw-text "..." (+ ex 10) (+ dy dh))
                   (send dc draw-text "(define find-help ..." (+ ex 10) (+ dy (* 2 dh)))
                   
                   (let ([y (+ dy (* 3 dh) (/ (- hh ih) 2))])
                     (send dc draw-text is (+ ex 10) y)
                     (send dc draw-text " ...))" (+ ex iw hw 10) y))

                   (send dc set-pen orangepen)
                   (up-connect (+ hx (/ hw 2)) (+ hy hn (/ hi 10))
                               (+ dx (* dw 0.6)) (+ dy (* dh 0.8)))

                   (connect (+ dx (* dw 0.4)) (+ dy (* dh 2.2))
                            (+ hx (/ hw 2)) (+ hy hh (- (/ he 10)))))
                 

                 ;;Normal layout
                 (let*-values ([(tx ty) (values 5 5)]
                               [(tw th tn ti te)
                                (draw-unit cpd? tx ty
                                           "Programming Environment"
                                           ""
                                           '(700 400) ;;; Hack!
                                           "evaluate     find-help")]
                               [(ex ey) (values (+ tx 15) (+ ty tn ti 5))]
                               [(ew eh en ei ee)
                                (draw-unit #t ex ey
                                           "Interp"
                                           "find-help"
                                           '("(define ...)"
                                             "..."
                                             "(send frame show #t)")
                                           "evaluate")]
                               [(hx hy) (values (+ ex (* 1.2 ew)) (+ ey (* 0.6 eh)))]
                               [(hw hh hn hi he)
                                (draw-unit second? hx hy
                                           "Help"
                                           "evaluate"
                                           '(""
                                             "..."
                                             "")
                                           "find-help")])
                   
                   (when link?
                     (send dc set-pen orangepen)
                     (connect (+ hx (/ hw 2)) (+ hy hn (/ hi 10))
                              (+ ex (/ ew 2)) (+ ey eh (- (/ ee 10))))
                     (connect (+ ex (/ ew 2)) (+ ey en (/ ei 10))
                              (+ hx (/ hw 2)) (+ hy hh (- (/ he 10)))))

                   (when propagate?
                     (up-connect (+ tx (* 1/3 tw)) (+ ty th (- (* te 0.9)))
                                 (+ ex (/ ew 2)) (+ ey eh (- (/ ee 10))))
                     (up-connect (+ tx (* 4/5 tw)) (+ ty th (- (* te 0.9)))
                                 (+ hx (/ hw 2)) (+ hy hh (- (/ he 10)))))))
             
             (send dc set-pen orig-p)
             (send dc set-brush orig-b)
             (send dc set-transformation t))
           500 400)
       -40 0 0 0))))

(define example-unit (gen #t #t #t #t #t))

