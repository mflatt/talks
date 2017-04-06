#lang slideshow
(require "castle.ss"
         "princess.ss"
         "well.ss"
         "thought.ss"
         "poster.ss"
         "things.ss"
         slideshow/play)

(provide movie-slides
         kingdom
         million-well
         plt-bm
         play-it
         clip-to-screen)

(define all-screen (inset full-page margin))

(define (clip-to-screen p)
  (inset
   (clip (refocus (cc-superimpose all-screen p)
                  all-screen))
   (- margin)))

(define (princess-in-castle n)
  (let-values ([(x y w h) (send window-path get-bounding-box)]
               [(p) (make-princess)])
    (let ([pw (pict-width p)]
          [ph (pict-height p)])
      (let ([dx (/ (- w pw) 2)]
            [dy (* n -15)])
        (pin-over castle
                  (+ x dx) (+ y dy)
                  (make-princess #:clip-body window-path
                                 #:clip-dx (- (+ x dx))
                                 #:clip-dy (- (+ y dy))))))))

(define kingdom
  (let ([well (scale well 0.25)])
    (cb-superimpose
     (inset castle 0 0 0 40)
     (hc-append
      gap-size
      well
      (vc-append
       (hc-append
        (* 8 gap-size)
        well
        (inset well 0 20 0 0))
       (hc-append
        (* 2 gap-size)
        (inset well 0 -30 0 0)
        well
        (inset well 0 20 0 0)
        well))
      (inset well 0 0 0 40)))))

#;
(slide (hb-append gap-size
                  (frame (scale (make-princess #:side 'right) 2))
                  (frame (scale (make-princess #:side 'left) 2))
                  castle
                  (scale (make-princess) 2)
                  well+sign))

(define p (make-princess))

(define (play-it #:name [name #f] times proc #:skip-last? [skip-last? #f])
  (let loop ([times times]
             [ns (map (lambda (n) 0.0) times)]
             [prevs null])
    (if (null? ns)
        (unless skip-last?
          (slide #:name name (apply proc prevs)))
        (let* ([a (car times)]
               [ns (cdr ns)]
               [N (car a)])
          (for ([i (in-range N)])
            (slide (apply proc (append prevs (list (/ i N)) ns))
                   #:name name
                   #:timeout (cadr a)))
          (let ([prevs (cons 1.0 prevs)])
            (loop (cdr times) ns prevs))))))

(define-syntax-rule (play-all #:name t ([id0 steps0 delay0] ...) ([id steps delay] ...) expr . rest)
  (play-it
   #:name t
   '([steps delay] ...)
   (lambda (id ...) 
     (let ([id0 1.0] ...)
       expr))
   . rest))

(define plt-bm (bitmap (collection-file-path "PLT-206.png" "icons")))

(define (toward-well* title-slide sign-n hooray-n n01 n01.5 n02 n03 n n2 n3 n4 extra-dx leg-n catch? step? make-princess f)
  (define n0 (+ n01 n02 n03))
  (let* ([princess (make-princess #:side (if (or (= n4 1.0)
                                                 (< 0.0 n01.5 1.0)
                                                 (and (= 1.0 n01) (= 0.0 n01.5)))
                                             'front
                                             'right)
                                  #:arm-angle (if (or (= n 1.0) (> n2 0.0))
                                                  (if (= n3 0.0)
                                                      0.0
                                                      (if catch? 
                                                          (* pi 4/10) 
                                                          (* (/ pi 10) (sin (* 10 leg-n)))))
                                                  (* (/ pi 10) (sin (* 10 (if (= 3.0 n0) n n0)))))
                                  #:front-arm-angle (if (zero? hooray-n)
                                                        0.0
                                                        (/ pi 2))
                                  #:leg-angle (if (= leg-n 1.0)
                                                  0.0
                                                  (* (/ pi 10) (sin (* 10 (if (or step? (= 3.0 n0)) leg-n n0))))))]
         [princess (inset princess 0 (- (* 10 (sin (* pi hooray-n)))) 0 0)])
    (values
     (let ([p (hb-append
               gap-size
               (scale
                (pin-over
                 (ht-append (blank 120 0) (ghost p))
                 (+ (* n 100) extra-dx (* (- 1 n01) -175))
                 (* (sin (* n3 pi)) -10)
                 (f princess))
                2.0)
               (let ([target (ghost (launder well+sign))])
                 (refocus
                  (lc-superimpose 
                   target
                   (inset (well+sign* sign-n) (* (- 1 n03) 440) 0 0 0))
                  target)))])
       (if title-slide
           (refocus
            (cc-superimpose
             (let* ([target (ghost title-slide)]
                    [offset (* n02 -1024)])
               (if (= n02 1.0)
                   target
                   (refocus
                    (lc-superimpose
                     target
                     (inset title-slide offset 0 0 0))
                    target)))
             p)
            p)
           p))
     princess)))

(define (toward-well title-slide sign-n hooray-n n01 n01.5 n02 n03 n n2 n3 n4 extra-dx leg-n catch? step? make-princess f)
  (let-values ([(p princess) (toward-well* title-slide sign-n hooray-n n01 n01.5 n02 n03 n n2 n3 n4 extra-dx leg-n 
                                           catch? step? make-princess f)])
    p))

(define-syntax-rule (play-all/well #:name t (pre-step ...) (n02 n03 n n2 n3 n4) (step ...) (more-step ...) expr ...)
  (play-all #:name t
            ()
            (pre-step
             ...            
             [n02 20 0.05]
             [n03 22 0.05]
             [n 20 0.05]
             [n2 1 0.65]
             [n3 5 0.05]
             [n4 1 0.5]
             step ...
             more-step ...)
            expr ...))

(define (castle-thought princess castle wish-n castle-n wish-granted-n fade-n)
  (thought
   (pin-over
    princess
    (- (* 1.5 (pict-width castle)))
    (- (* 0.5 (pict-height castle)))
    (cellophane castle (min castle-n (- 1 fade-n))))
   princess
   castle
   wish-n
   (fast-start wish-granted-n)
   #:wrap-thought (lambda (t)
                    (cellophane t (- 1 fade-n)))))


(define (million-thought p princess amt thought-grow-n million-thought-n thought-gone-n fade-n)
  (let-values ([(cx cy) (cc-find p one-in-sign)]
               [(million) (let ([p (million-text (/ pi 10))]
                                [p2 (million-text (/ pi 10) #:amt amt)])
                            (cc-superimpose
                             (poster (million-text 0) p (/ pi 10))
                             p2))])
    (thought (pin-over p
                       (- cx (/ (pict-width million) 2))
                       (- cy (/ (pict-height million) 2))
                       (cellophane million (min million-thought-n (- 1 fade-n))))
             princess 
             million
             thought-grow-n 
             thought-gone-n
             #:wrap-thought (lambda (t)
                              (cellophane t (- 1 fade-n))))))

(define (white-out p n)
  (if (zero? n)
      p
      (cc-superimpose
       p
       (cellophane (colorize (filled-rectangle client-w client-h) "white")
                   n))))

(define (princess1-slides title-slide)
  (play-all/well
   #:name "Princess 1"
   ([n00 1 #f]
    [n01 17 0.05]
    [pause1-n 1 #f]
    [n01.5 10 0.05])
   (n02
    n03
    n 
    n2
    n3
    n4)
   ()
   ([pre-sign-pause 1 #f]
    [sign-n 10 0.05]
    [pre-wish-pause-n 1 #f]
    [thought1-n 10 0.05]
    [castle-thought-n 10 0.05]
    [castle-wish-wait 1 0.5]
    [thought3-n 10 0.05]
    [to-castle-n 20 0.05]
    [castle-pause-n 1 0.25]
    [in-castle-n 10 0.05]
    [the-end 1 #f]
    [fade-out-n 5 0.05]
    [final-pause-n 1 0.25])
   (clip-to-screen
    (white-out
     (toward-well
      title-slide sign-n 0.0
      n01 n01.5 n02 n03 n n2 n3 n4
      0 n #t #f
      make-princess
      (lambda (princess)
        (let* ([castle (scale (if (= in-castle-n 0.0)
                                  castle
                                  (princess-in-castle (fast-start in-castle-n)))
                              0.5)]
               [pre-wish (castle-thought princess castle
                                         thought1-n castle-thought-n thought3-n
                                         0.0)])
          (cond
           [(zero? to-castle-n) pre-wish]
           [else (let ([p (ghost pre-wish)])
                   (let-values ([(cx cy) (lt-find p castle)]
                                [(px py) (lb-find p princess)]
                                [(p2) (make-princess #:side 'left
                                                     #:arm-angle (* (/ pi 10) (sin (* 10 to-castle-n)))
                                                     #:leg-angle (* (/ pi 10) (sin (* 20 to-castle-n))))])
                     ((if (to-castle-n . > . 0.8) 
                          (if (= to-castle-n 1.0)
                              (lambda (p p2 x y) p)
                              pin-under )
                          pin-over)
                      (pin-over
                       p
                       (+ cx (* to-castle-n (pict-width castle) 1/4)) 
                       (- cy (* to-castle-n (pict-height castle) 1/4))
                       (scale castle (+ (* to-castle-n 0.5) 1.0)))
                      (- px (* to-castle-n (- px (+ cx (* (pict-width castle) 0.75))))) 
                      (+ py (* to-castle-n 0.25 gap-size))
                      (scale (inset p2 0 (- (pict-height p2)) 0 0) (- 1 (* to-castle-n 0.75))))))]))))
     fade-out-n))
   #:skip-last? #t))

(define (princess3-slides)
  (play-all/well
   #:name "Princess 3"
   ([fade-in-n 5 0.05]
    [pre-walk-pause-n 1 0.5]
    [n01.5 1 0.25])
   (n02
    n03
    n 
    n2
    n3
    n4)
   ([wish-pause-n 1 #f]
    [castle-wish-n 10 0.05]
    [castle-wish-no-n 10 0.05]
    [castle-wish-fade-n 5 0.05]
    [no-pause1-n 1 0.5]
    [million-wish-n 10 0.05]
    [million-wish-no-n 10 0.05]
    [million-wish-fade-n 5 0.05]
    [no-pause2-n 1 0.5]
    [kingdom-wish-pause 1 #f]
    [kingdom-wish-thought-n 10 0.05]
    [kingdom-wish-n 10 0.05]
    [kingdom-wish-pause-n 1 0.5])
   ([kingdom-wish-granted-n 10 0.05]
    [kingdom-pause-n 1 #f]
    [walk1-n 20 0.05]
    [wish1-grow-n 10 0.05]
    [wish1-done-n 10 0.05]
    [wish1-drop-n 10 0.05]
    [wish1-walk-n 10 0.05]
    [wish2-grow-n 10 0.05]
    [wish2-done-n 10 0.05]
    [wish2-drop-n 10 0.05]
    [wish2-walk-n 10 0.05]
    [wish3-grow-n 10 0.05]
    [wish3-done-n 10 0.05]
    [wish3-drop-n 10 0.05]
    [wish3-walk-n 10 0.05]
    [jump-pause-n 1 0.75]
    [hooray-n 10 0.05]
    [the-end-n 1 #f]
    [the-end-in-n 5 0.05]
    [fade-out-n 5 0.05])
   (clip-to-screen
    (cc-superimpose
     (white-out
      (let-values ([(wish-no-n wish-fade-n)
                    (if (zero? million-wish-n)
                        (values castle-wish-no-n castle-wish-fade-n)
                        (values million-wish-no-n million-wish-fade-n))])
        (let-values ([(p princess)
                      (toward-well*
                       #f 1.0 hooray-n
                       1.0 n01.5 n02 
                       (if (= 1.0 n4)
                           (- 1 walk1-n)
                           n03)
                       n 
                       n2 n3 n4
                       0 (+ n
                            walk1-n
                            (/ (+ wish1-grow-n wish1-done-n 
                                  wish1-drop-n wish1-walk-n
                                  wish2-grow-n wish2-done-n 
                                  wish2-drop-n wish2-walk-n
                                  wish3-grow-n wish3-done-n 
                                  wish3-drop-n wish3-walk-n)
                               2))
                       (or (< 0.0 wish1-walk-n 1.0)
                           (< 0.0 wish2-walk-n 1.0)
                           (< 0.0 wish3-walk-n 1.0))
                       (positive? walk1-n)
                       (make-keyword-procedure
                        (lambda (kws kw-vals . args)
                          (keyword-apply make-princess 
                                         (remq '#:side kws)
                                         (remk #'side kws kw-vals)
                                         #:dress "violet"
                                         #:hair "gold"
                                         #:smile? (not (and (< 0.0 wish-no-n)
                                                            (< wish-fade-n 1.0)))
                                         #:shake (* 2 (sin (* 2 pi wish-no-n)))
                                         #:side (if (or (zero? walk1-n)
                                                        (= 1 wish3-walk-n))
                                                    (assk '#:side kws kw-vals)
                                                    'left)
                                         args)))
                       (lambda (princess)
                         (let* ([castle (scale castle 0.5)]
                                [pre-wish (castle-thought princess castle
                                                          castle-wish-n castle-wish-n 0.0
                                                          castle-wish-fade-n)]
                                [drop-n (+ wish1-drop-n wish1-walk-n)]
                                [focus-pre-wish pre-wish]
                                [pre-wish (inset pre-wish 
                                                 (* (pict-width princess) 2)
                                                 (* (pict-height princess) 0.5)
                                                 0
                                                 0)]
                                [pre-wish (wish-thought pre-wish
                                                        princess
                                                        (scale diamond 0.5)
                                                        wish1-grow-n
                                                        wish1-done-n
                                                        drop-n
                                                        drop-n)]
                                [drop-n (+ wish2-drop-n wish2-walk-n)]
                                [pre-wish (wish-thought pre-wish 
                                                        princess
                                                        (scale ice-cream 0.5)
                                                        wish2-grow-n
                                                        wish2-done-n
                                                        drop-n
                                                        drop-n)]
                                [drop-n (+ wish3-drop-n wish3-walk-n)]
                                [pre-wish (wish-thought pre-wish 
                                                        princess
                                                        (scale fishbowl 0.5)
                                                        wish3-grow-n
                                                        wish3-done-n
                                                        drop-n
                                                        drop-n)])
                           (refocus pre-wish focus-pre-wish))))])
          (let ([p (million-thought p
                                    princess
                                    "1,000,000"
                                    million-wish-n
                                    million-wish-n
                                    0.0
                                    million-wish-fade-n)]
                [kingdom (let* ([k (scale kingdom 0.5)]
                                [target (ghost (launder k))])
                           (if (= walk1-n 1.0)
                               target
                               (refocus
                                (lt-superimpose
                                 target
                                 (inset k (* walk1-n -350) (* walk1-n -100) 0 0))
                                target)))])
            (let ([p2 (castle-thought (ghost princess) kingdom
                                      kingdom-wish-thought-n kingdom-wish-n
                                      kingdom-wish-granted-n
                                      0.0)])
              (let-values ([(x y) (lt-find p princess)])
                (add-well
                 (add-well
                  (add-well (pin-under p x y p2)
                            (+ wish1-grow-n wish1-done-n 
                               wish1-drop-n wish1-walk-n))
                  (+ wish2-grow-n wish2-done-n 
                     wish2-drop-n wish2-walk-n))
                 (+ wish3-grow-n wish3-done-n 
                    wish3-drop-n wish3-walk-n)))))))
      (if (= 1 fade-in-n n)
          fade-out-n
          (- 1 fade-in-n)))
     (cellophane
      (scale
       (vc-append
        (- (/ gap-size 2))
        (text "The End" "Brush Script MT, Italic" 42))
       2)
      the-end-in-n)))))

(define (add-well p n)
  (if (or (zero? n)
          (n . > . 4.0))
      p
      (let ([well (scale well 0.75)])
        (refocus
         (cc-superimpose
          (pin-over titleless-page
                    (- (* n 3/8 client-w) (pict-width well) 10)
                    (- (/ client-h 2) (pict-height well))
                    well)
          p)
         p))))

(define (million-text rotation #:amt [amt "1,000,000"])
  (text amt 'roman (current-font-size) rotation))

(define-syntax play-all/well/cycle 
  (syntax-rules ()
    [(_ #:name t (pre ...) (id ...) (post ...) () (after ...) expr ...)
     (play-all/well #:name t (pre ...) (id ...) () (post ... after ...) expr ...)]
    [(_ #:name t (pre ...) (id ...) (post ...) ([return-n] . rest) (after ...) expr ...)
     (play-all/well/cycle
      #:name t 
      (pre ...)
      (id ...)
      (post ...
            [return-n 10 0.05])
      rest
      (after ...)
      expr ...)]
    [(_ #:name t (pre ...) (id ...) (post ...) (wish-grow-n wish-gone-n wish-drop-n wish-back-n . rest) (after ...) expr ...)
     (play-all/well/cycle
      #:name t 
      (pre ...)
      (id ...)
      (post ...
            [wish-grow-n 10 0.05]
            [wish-gone-n 10 0.05]
            [wish-drop-n 10 0.05]
            [wish-back-n 10 0.05])
      rest
      (after ...)
      expr ...)]))

(define (pick n n2 a b c)
  (if (zero? n) a (if (zero? n2) b c)))

(define (remk kw kws kw-vals)
  (let loop ([kws kws][kw-vals kw-vals])
    (if (null? kws)
        null
        (if (eq? (car kws) kw)
            (cdr kw-vals)
            (cons (car kw-vals) (loop (cdr kws) (cdr kw-vals)))))))

(define (assk kw kws kw-vals)
  (let loop ([kws kws][kw-vals kw-vals])
    (if (eq? (car kws) kw)
        (car kw-vals)
        (loop (cdr kws) (cdr kw-vals)))))

(define (princess2-slides)
  (play-all/well/cycle
   #:name "Princess 2"
   ([fade-in-n 5 0.05]
    [pre-walk-pause-n 1 0.5]
    [n01.5 1 0.5])
   (n02
    n03
    n 
    n2
    n3
    n4)
   ([wish-pause-n 1 #f]
    [thought-grow-n 10 0.05]
    [million-thought-n 10 0.05]
    [million-wish-wait 1 1.0]
    [thought-gone-n 10 0.05])
   (wish1-grow-n
    wish1-gone-n
    wish1-drop-n
    wish1-back-n
    [wish2-return-n]
    wish2-grow-n
    wish2-gone-n
    wish2-drop-n
    wish2-back-n
    [wish3-return-n]
    wish3-grow-n
    wish3-gone-n
    wish3-drop-n
    wish3-back-n
    [wish4-return-n])
   ([jump-pause-n 1 0.75]
    [hooray-n 10 0.05]
    [pause-n 1 #f]
    [fade-out-n 5 0.05]
    [transition-pause-n 1 0.25])
   (clip-to-screen
    (white-out
     (let* ([wish-grow-n (if (zero? wish4-return-n)
                             (pick wish2-return-n wish3-return-n wish1-grow-n wish2-grow-n wish3-grow-n)
                             0.0)]
            [wish-gone-n (pick wish2-return-n wish3-return-n wish1-gone-n wish2-gone-n wish3-gone-n)]
            [drop-n (if (zero? wish4-return-n)
                        (pick wish2-return-n wish3-return-n wish1-drop-n wish2-drop-n wish3-drop-n)
                        0.0)]
            [back-n (if (zero? wish4-return-n)
                        (pick wish2-return-n wish3-return-n wish1-back-n wish2-back-n wish3-back-n)
                        0.0)]
            [returning? (or (< 0.0 wish2-return-n 1.0)
                            (< 0.0 wish3-return-n 1.0))]
            [return-n (if (< 0.0 wish2-return-n 1.0)
                          wish2-return-n
                          wish3-return-n)]
            [gen-n (if (zero? wish4-return-n)
                       (if returning?
                           return-n
                           (min n (- 1 back-n)))
                       0.0)])
       (let-values ([(p princess) (toward-well* #f 1.0 hooray-n
                                                1.0 n01.5 n02 n03
                                                gen-n
                                                n2 n3 n4
                                                (+ (* back-n -100)
                                                   (* drop-n -70)
                                                   (if returning?
                                                       (* (- 1 return-n) -170)
                                                       0.0)
                                                   (if (zero? wish4-return-n)
                                                       0.0
                                                       (* (- 1 wish4-return-n) -170)))
                                                (if (= gen-n 1.0) drop-n gen-n)
                                                #t #f
                                                (make-keyword-procedure
                                                 (lambda (kws kw-vals . args)
                                                   (keyword-apply make-princess 
                                                                  (remq '#:side kws)
                                                                  (remk #'side kws kw-vals)
                                                                  #:side (if (or returning?
                                                                                 (< 0.0 wish4-return-n 1.0))
                                                                             'right
                                                                             (if (zero? drop-n)
                                                                                 (assk '#:side kws kw-vals)
                                                                                 'left))
                                                                  #:dress "lightblue" 
                                                                  #:hair "peru" 
                                                                  args)))
                                                values)])
         (let ([p (million-thought
                   p
                   princess
                   (cond
                    [(positive? wish3-gone-n) "999,997"]
                    [(positive? wish2-gone-n) "999,998"]
                    [(positive? wish1-gone-n) "999,999"]
                    [else "1,000,000"])
                   thought-grow-n
                   million-thought-n
                   thought-gone-n
                   0.0)])
           (wish-thought
            p
            princess
            (cond
             [(zero? wish2-return-n) diamond]
             [(zero? wish3-return-n) ice-cream]
             [else fishbowl])
            wish-grow-n
            wish-gone-n
            drop-n
            0.0))))
     (max (- 1 fade-in-n)
          fade-out-n)))
   #:skip-last? #t))

(define (wish-thought p princess wish wish-grow-n wish-gone-n drop-n x-drop-n)
  (let-values ([(wish-x princess-x)
                (let-values ([(x y) (lt-find p princess)])
                  (values (* x x-drop-n 0.5)
                          (- x (/ (pict-width wish) 2))))])
    (if (and (positive? x-drop-n)
             (wish-x . > . princess-x))
        p
        (let* ([wish-y (* drop-n (- (pict-height p) (* (pict-height princess) 3/4) (pict-height wish)))]
               [wish-x (min wish-x princess-x)]
               [wish-n (max 0.0 (- (* 2 wish-grow-n) 1.0))])
          (thought
           (pin-over 
            p
            wish-x wish-y
            (cellophane wish wish-n))
           princess
           wish
           wish-grow-n
           wish-gone-n)))))

(define million-well
  (million-thought well+sign well+sign "1,000,000" 0.0 1.0 0.0 0.0))

(define (movie-slides title-slide)
  (princess1-slides title-slide)
  (princess2-slides)
  (princess3-slides))
