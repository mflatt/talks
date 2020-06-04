#lang slideshow
(require racket/draw
         racket/class
         "color.rkt")

(provide person
         
         students
         make-students
         group-students
         
         professor
         accountant
         reviewer)

(define ovalish
  (let ([p (new dc-path%)])
    (define r 20)
    (define r2 15)
    (send p move-to 45 0)
    (send p curve-to
          (- 90 r) 0
          90 r
          90 50)
    (send p curve-to
          90 (- 100 r2)
          (- 90 r2) 100
          45 100)
    (send p curve-to
          r2 100
          0 (- 100 r2)
          0 50)
    (send p curve-to
          0 r
          r 0
          45 0)
    (send p close)
    p))

(define puffy-hair
  (let ([p (new dc-path%)])
    (send p move-to
          0 20)
    (send p curve-to
          0 0
          25 0
          25 5)
    (send p curve-to
          25 -5
          45 -10
          45 0)
    (send p curve-to
          45 -10
          65 -5
          65 5)
    (send p curve-to
          65 0
          90 0
          90 20)
    (send p curve-to
          90 30
          45 30
          45 20)
    (send p curve-to
          45 30
          0 30
          0 20)
    (send p close)
    p))

(define parted-hair
  (let ([p (new dc-path%)])
    (send p move-to
          0 20)
    (send p curve-to
          0 -20
          90 -20
          90 20)
    (send p curve-to
          90 30
          45 30
          45 20)
    (send p curve-to
          45 30
          0 30
          0 20)
    (send p close)
    p))

(define bangs
  (let ([p (new dc-path%)])
    (send p move-to
          0 30)
    (send p curve-to
          0 -20
          90 -20
          90 30)
    (send p close)
    p))

(define parted-left
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p curve-to 30 15 35 15 40 13)
    (send p curve-to 40 20 70 20 75 12)
    (send p close)
    (send p scale 1.6 3)
    (send p translate -35 -23)
    p))

(define parted-right
  (let ([p (new dc-path%)])
    (send p append parted-left)
    (send p scale -1 1)
    (send p translate 90 0)
    p))

(define spiky
  (let ([p (new dc-path%)])
    (send p move-to 25 22)
    (send p curve-to
          10 22
          4 22
          4 35)
    (send p line-to 3 22)
    (for ([i (in-range 20 90 10)])
      (define dy (- (* (abs (- 45 i)) 0.3)))
      (send p line-to (- i 3) (- -10 dy))
      (send p line-to (+ i 3) (- 0 dy)))
    (send p line-to 87 22)
    (send p line-to 86 35)
    (send p curve-to
          86 22
          80 22
          65 22)
    (send p close)
    p))

(define balding
  (let ([p (new dc-path%)])
    (send p rectangle 0 10 10 20)
    (send p rotate (* -0.22 pi))
    (send p translate 20 0)
    (let ([p2 (new dc-path%)])
      (send p2 append p)
      (send p scale -1 1)
      (send p translate 90 0)
      (send p append p2))
    p))

(define swoop
  (let ([p (new dc-path%)])
    (send p move-to 50 0)
    (send p curve-to -20 0 -10 60 -10 80)
    (send p curve-to -10 90 -20 80 -20 75)
    (send p curve-to -30 80 -20 90 -10 90)
    (send p curve-to 20 90 15 60 50 50)
    (send p close)
    (let ([p2 (new dc-path%)])
      (send p2 append p)
      (send p2 scale -1 1)
      (send p2 translate 100 0)
      (send p append p2))
    (send p scale 0.9 1)
    p))

(define straight
  (let ([p (new dc-path%)])
    (send p move-to 50 0)
    (send p curve-to -20 0 -10 60 -10 80)
    (send p curve-to 20 90 15 60 50 50)
    (send p close)
    (let ([p2 (new dc-path%)])
      (send p2 append p)
      (send p2 scale -1 1)
      (send p2 translate 100 0)
      (send p append p2))
    (send p scale 0.9 1)
    p))

(define motarboard
  (let ([p (new dc-path%)])
    (send p move-to 0 10)
    (send p line-to 45 -10)
    (send p line-to 90 10)
    (send p line-to 45 30)
    (send p close)
    (send p move-to 7 12)
    (send p line-to 5 50)
    (send p line-to 7 50)
    (send p line-to 8 13)
    (send p close)
    p))

(define (person #:hair-style [hair-style 'puffy]
                #:glasses? [glasses? #f]
                #:mouth-style [mouth-style #f]
                #:hat-style [hat-style #f])
  (define no-pen (make-pen #:style 'transparent))
  (define glasses-pen (make-pen #:width 3))
  (define mouth-pen (make-pen #:width 3 #:color "brown"))
  (define face-brush (make-brush #:color person-color))
  (define eye-brush (make-brush #:color "white"))
  (define pupil-brush (make-brush #:color "brown"))
  (define hair-brush (make-brush #:color "brown"))
  (define black-brush (make-brush #:color "black"))
  (define no-brush (make-brush #:style 'transparent))
  (dc (lambda (dc x y)
        (define old-pen (send dc get-pen))
        (define old-brush (send dc get-brush))
        (send dc set-pen no-pen)

        (case hair-style
          [(swoop)
           (send dc set-brush hair-brush)
           (send dc draw-path swoop x y)]
          [(straight)
           (send dc set-brush hair-brush)
           (send dc draw-path straight x y)])

        (send dc set-brush face-brush)
        (send dc draw-path ovalish x y)

        (send dc set-brush eye-brush)
        (send dc draw-ellipse (+ x 20) (+ y 40) 20 10)
        (send dc draw-ellipse (+ x 50) (+ y 40) 20 10)

        (send dc set-brush pupil-brush)
        (send dc draw-ellipse (+ x 25) (+ y 40) 10 10)
        (send dc draw-ellipse (+ x 55) (+ y 40) 10 10)

        (send dc set-brush hair-brush)
        (case hair-style
          [(#f) (void)]
          [(swoop)
           (send dc draw-path parted-hair x y)]
          [(straight)
           (send dc draw-path bangs x y)]
          [(parted-left)
           (send dc draw-path parted-left x y)]
          [(parted-right)
           (send dc draw-path parted-right x y)]
          [(spiky)
           (send dc draw-path spiky x y)]
          [(balding)
           (send dc draw-path balding x y)]
          [else
           (send dc draw-path puffy-hair x y)])

        (case hat-style
          [(mortarboard)
           (send dc set-brush black-brush)
           (send dc draw-path motarboard x y)]
          [(high-mortarboard)
           (send dc set-brush black-brush)
           (send dc draw-path motarboard x (- y 10))]
          [(#f) (void)])

        (send dc set-brush no-brush)

        (when glasses?
          (send dc set-pen glasses-pen)

          (send dc draw-ellipse (+ x 17) (+ y 32) 26 26)
          (send dc draw-ellipse (+ x 48) (+ y 32) 26 26)
          (send dc draw-line (+ x 43) (+ y 45) (+ x 47) (+ y 45))
          (send dc draw-line (+ x 0) (+ y 38) (+ x 17) (+ y 43))
          (send dc draw-line (+ x 90) (+ y 38) (+ x 73) (+ y 43)))

        (case mouth-style
          [(smile)
           (send dc set-pen mouth-pen)
           (send dc draw-arc
                 (+ x 25) (+ y 55)
                 40 20
                 (* -3/4 pi) (* -1/4 pi))])

        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
      90 100))

(define (make-students #:count [count 5])
  (case count
    [(1)
     (list (person #:hair-style 'swoop))]
    [(2)
     (list
      (person #:hair-style 'straight)
      (person #:hair-style 'parted-right))]
    [(3)
     (list
      (person #:hair-style 'straight)
      (person #:hair-style 'parted-right)
      (person))]
    [(5)
     (list
      (person)
      (person #:hair-style 'swoop)
      (person #:hair-style 'parted-left)
      (person #:hair-style 'spiky)
      (person #:hair-style 'straight))]
    [else
     (error 'make-students "unsupported count: ~v" count)]))

(define (group-students l)
  (define sep (* 1.5 gap-size (/ (pict-width (car l)) 90)))
  (cond
    [(= 5 (length l))
     (define-values (a b c d e) (apply values l))
     (vc-append
      (hc-append sep a b c)
      (hc-append sep d e))]
    [(= 3 (length l))
     (define-values (a b c) (apply values l))
     (vc-append
      (hc-append sep a b)
      c)]
    [else
     (apply hc-append sep l)]))
    
(define (students #:count [count 5])
  (group-students (make-students #:count count)))

(define (accountant)
  (person #:hair-style 'balding
          #:glasses? #t))

(define (reviewer #:happy? [happy? #f])
  (person #:hair-style 'straight
          #:glasses? #t
          #:mouth-style (and happy? 'smile)))

(define (professor)
  (person #:hair-style #f
          #:hat-style 'mortarboard))

(module+ main
  (slide (students #:count 3)))
