#lang racket
(require pict3d
         pict3d/universe
         pict3d-die-cut
         racket/draw
         (only-in racket/gui/base
                  hide-cursor-until-moved
                  message-box)
         "cover.rkt")

(require drracket/private/dock-icon)
(set-dock-tile-bitmap (read-bitmap "sphere.png"))

(define mode 'movie)

(define (timeinfo ti key)
  (cond
    [(number? ti) ti]
    [else
     (define t (or (hash-ref ti key #f)
                   (hash-ref ti 'now)))
     (define sm (hash-ref ti 'slo-mo #f))
     (if (and sm
              (sm . < . t))
         (+ sm (* (- t sm) 2/3))
         t)]))
(define (syncinfo ti key [default #f])
  (hash-ref ti key default))

(define-syntax-rule (define-*/1 op* op)
  (define (op* p a)
    (if (procedure? p)
        (lambda (t) (op (p t) a))
        (op p a))))

(define-*/1 move* move)
(define-*/1 move*-x move-x)
(define-*/1 move*-y move-y)
(define-*/1 move*-z move-z)
(define-*/1 scale*-x scale-x)

(define (combine* . l)
  (define ps (let loop ([l l])
               (cond
                 [(null? l) null]
                 [(list? (car l)) (loop (append (car l) (cdr l)))]
                 [else (cons (car l) (loop (cdr l)))])))
  (if (andmap pict3d? ps)
      (combine ps)
      (lambda (t)
        (combine (for/list ([p (in-list ps)])
                   (if (procedure? p)
                       (p t)
                       p))))))

(define lights+camera
  (combine (sunlight (dir -0.2 0 -1))
           (basis 'camera 
                  (point-at (pos 150 -50 180) origin
                            #:up +y))))

(define (topple ti)
  (define go-dt (- (timeinfo ti 'now)
                   (timeinfo ti 'topple)))
  (define STRAIGHT-T 2000)
  (define straight-rot (/ (min STRAIGHT-T go-dt) 5))
  (define straight-amt (* 2 (/ (- STRAIGHT-T (min STRAIGHT-T go-dt)) STRAIGHT-T)))
  (define base-dt (max 0 (- go-dt STRAIGHT-T)))
  (define ROLL-T 2000)
  (define init-amt (- 1 (cos (* 1/2 pi (- 1 (/ (min ROLL-T base-dt) ROLL-T))))))
  (define init-rot (+ (* -180 init-amt) straight-rot))
  (define dt (max 0 (- base-dt ROLL-T -800)))
  (define dtx (* (/ (expt (min dt 5000) 3.0)
                    (expt 5000.0 3.0))
                 5000.0))
  (define dh (/ dtx 5000))
  (define rev-cover (rotate-z cover (+ 180 (- init-rot) (* 0.9 (/ dtx -5)))))
  (define a (+ 60 (* -60 dh) 30 (* -30 (/ (min ROLL-T base-dt) ROLL-T))))
  (define spun-cover (rotate-x rev-cover a))
  (combine
   (set-color (quad (pos -1500 1500 -10) (pos 1500 1500 -10) (pos 1500 -1500 -10) (pos -1500 -1500 -10) #:back? #t)
              (rgba 0.2 0.2 0.2 1))
   (move-x
    (set-color (cylinder (pos (- RR) (- RR) -5) (pos RR RR -5.1))
               (rgba 0 0 0 1))
    (* (+ init-amt straight-amt) 200))
   (move-y
    (move-y (move-z (rotate-z spun-cover (- (/ dtx -5)))
                    (* RR (sin (* pi (/ a 180.0)))))
            (min 0 (+ (max 0 (- (/ dtx 20) 50)) (* -1/2 RR))))
    (* -20 (- 1 (/ (min (+ base-dt go-dt) (+ STRAIGHT-T ROLL-T)) (+ STRAIGHT-T ROLL-T)))))))
    

(define tt-font (make-font #:family 'modern #:weight 'bold #:size 6))

(define (make-sign str col)
  (define (half c) (+ c (* 1/2 (- 1 c))))
  (freeze
   (rotate-x (scale
              (set-color (die-cut-text str
                                       #:font tt-font
                                       #:expected-scale 10
                                       #:depth 0)
                         col)
              0.2)
             90)))

(define plain-c (freeze (cube origin 3)))
(define broke-c (freeze (sphere origin 3)))

(define br-ms 600)
(define program-break-period (* br-ms 9))

(define tr-color (rgba 0.0 0.0 0.8 1.0))

(define (make-dissolver plain-c broke-c to-pipe time-sym dn)
  (define dt (* dn br-ms))
  (move* (lambda (ti)
           (let* ([t (- (timeinfo ti 'now) (timeinfo ti time-sym))]
                  [t (- (modulo (floor t) program-break-period) dt)])
             (cond
               [(t . <= . 0) plain-c]
               [(< 0 t br-ms) (move broke-c (dir-scale to-pipe (/ t br-ms)))]
               [else empty-pict3d])))
         (dir-scale to-pipe -1)))
         
(define (make-program sign #:dissolve [dissolve 'dissolve])
  (define W 4)
  (define mk-c (lambda (dx dy)
                 (define dn (+ dx (* dy W)))
                 (define to-pipe (dir (* (+ 0.5 (- dx (/ W 2))) 5)
                                      0
                                      (* dy -5)))
                 (make-dissolver plain-c broke-c to-pipe dissolve dn)))
  (define (mk-l dty)
    (combine* (mk-c 0 dty)
              (mk-c 1 dty)
              (mk-c 2 dty)
              (mk-c 3 dty)))
  (combine* (move-x (move-z (move-y sign -4) 12) -9)
            (mk-l 0)
            (mk-l 1)
            (mk-l 2)))

(define (topless-cube pos scale)
  (define panel (quad (pos+ pos (dir (- scale) (- scale) scale))
                      (pos+ pos (dir (- scale) (- scale) (- scale)))
                      (pos+ pos (dir scale (- scale) (- scale)))
                      (pos+ pos (dir scale (- scale) scale))))
  (combine panel
           (rotate-x panel 180)
           (rotate-z panel 90)
           (rotate-z panel -90)
           (rotate-x panel 90)))

(define big-plain-c (topless-cube origin 5))
(define big-broke-c (freeze (sphere origin 5)))

(define factory-roof
  (set-color (combine
              (triangle (pos -10 10 10)
                        (pos 10 10 20)
                        (pos 10 10 10))
              (triangle (pos -10 -10 10)
                        (pos 10 -10 10)
                        (pos 10 -10 20))
              (quad (pos 10 10 10)
                    (pos 10 10 20)
                    (pos 10 -10 20)
                    (pos 10 -10 10))
              (quad (pos -10 10 10)
                    (pos 10 10 20)
                    (pos 10 -10 20)
                    (pos -10 -10 10)
                    #:back? #t))
             (rgba 1 1 1 1)))

(define (segment dn)
  (define dx (+ 5 (* 20 (- 1 dn))))
  (define dN (* (- 2 dn) 8))
  (combine* (for*/list ([x 2] [y 2] [z 2])
              (make-dissolver big-plain-c big-broke-c
                              (dir (- dx (* x 10)) (+ 5 (* y -10)) (+ 5 (* z -10)))
                              'dissolve-factory (+ dN (+ (- 1 x) (* 4 y) (* 2 z)))))
            (move-x factory-roof (- 5 dx))))

(define factory-icon
  (move*-z
   (scale*-x (combine* (segment 0) (segment 1) (segment 2))
             0.8)
   12))
(define (factory* sign)
  (combine* factory-icon
            (move-x (move-z (move-y (scale sign 3) -12) 20) -20)))
(define (factory sign)
  ((factory* sign) 0))

(define init-home (dir 0 -160 10))
(define tr-program-sign (make-sign "#lang typed/racket" tr-color))
(define init-program (move* (make-program tr-program-sign) init-home))
(define init-program-solid (move* ((make-program tr-program-sign) 0) init-home))

(define cover-scale 1/20)
(define cover-home (dir+ init-home (dir 0 -20 0)))

(define (init-cover ti)
  (define dt (min 500 (- (timeinfo ti 'now) (timeinfo ti 'uncover))))
  (move-x (move-z (move (scale (rotate-z cover (/ dt -3)) cover-scale)
                        cover-home)
                  0.3)
          (/ dt 50)))

(define ground
  (combine
   (set-emitted (quad (pos -15000 1500 -100) (pos 15000 1500 -100) (pos 15000 1500 2000) (pos -15000 1500 2000) #:back? #f)
                (emitted "lightblue" 0.5))
   (set-color (quad (pos -1500 1500 0) (pos 1500 1500 0) (pos 1500 -1500 0) (pos -1500 -1500 0) #:back? #t)
              (rgba 0.8 1 0.8 1))
   (set-color (move (scale (cylinder (pos (- RR) (- RR) 2) (pos RR RR 3)) cover-scale)
                    cover-home)
              (rgba 0 0 0 1))))
   
(define tr-home (dir 0 0 0))
(define tr-factory (move* (factory* (make-sign "typed/racket" tr-color))
                          tr-home))

(define contract-color (rgba 0 0.8 0 1.0))

(define contracts-home (dir 140 100 0))
(define contracts-factory (move* (factory* (make-sign "racket/contract" contract-color)) contracts-home))

(define untyped-color (rgba 0.8 0 0 1))

(define racket-home (dir 210 -80 0))
(define base-racket-factory (factory (make-sign "racket" untyped-color)))
(define racket-factory (move* base-racket-factory racket-home))

(define another-racket-home (dir 0 120 0))
(define another-racket-factory (move* base-racket-factory another-racket-home))

(define untyped-home (dir 90 -80 0))
(define racket-sign (make-sign "#lang racket" untyped-color))
(define untyped-program-solid (move* ((make-program racket-sign) 0) untyped-home))
(define untyped-program (move* (make-program racket-sign #:dissolve 'untyped-dissolve) untyped-home))

(define untyped2-home (dir 210 -160 0))
(define untyped2-program (move* ((make-program racket-sign) 0) untyped2-home))

(define extra-color (rgba 0.8 0.0 0.8 1))
(define extra-program-home (dir -60 -80 0))
(define extra-sign tr-program-sign)
(define extra-program (move ((make-program extra-sign) 0) extra-program-home))
(define extra-factory-home (dir -60 80 0))
(define extra-factory (move* (factory (make-sign "typed/racket/gui" extra-color)) extra-factory-home))

(define algol-color (rgba 0.8 0.8 0.0 1))
(define algol-program-home (dir 180 -20 0))
(define algol-program2-home (dir 240 -20 0))
(define algol-sign (make-sign "#lang algol60" algol-color))
(define algol-program (move ((make-program algol-sign) 0) algol-program-home))
(define algol-program2 (move ((make-program racket-sign) 0) algol-program2-home))
(define algol-factory-home (dir 240 70 0))
(define algol-factory (move* (factory (make-sign "algol60" algol-color)) algol-factory-home))

(define leak-home (dir 45 -120 0))

(define question-text
  (move-x
   (freeze
    (set-emitted (die-cut-text "?"
                               #:font (make-font #:family 'roman #:weight 'bold #:size 8)
                               #:expected-scale 10
                               #:depth 0)
                 (emitted 1 1 0 1.5)))
   -2))
(define (make-question d salt)
  (move* (lambda (ti)
           (define now (timeinfo ti 'now))
           (define qt (- now (timeinfo ti 'question)))
           (define qdt (- now (timeinfo ti 'question-done)))
           (cond
             [(positive? qdt)
              (if (qdt . < . 250)
                  (rotate-y question-text (* 180 (/ qdt 250.0)))
                  empty-pict3d)]
             [(qt . < . 250)
              (rotate-y question-text (* 180 (/ (- 250 qt) 250.0)))]
             [else
              (define st (* (sin salt) (/ (- qt 250) 300.0)))
              (move-y (move-x question-text (* 1 (sin st)))
                      (* 1 (sin (* 1.3 st))))]))
         (dir+ leak-home d)))

(define questions
  (combine* (make-question (dir 5 8 0) 50.0)
            (make-question (dir -7 5 -1) 100.0)
            (make-question (dir 2 -4 -8) 200.0)))

(define (text-line l)
  (cond
   [(string? l) (die-cut-text/size l)]
   [else
    (for/fold ([p empty-pict3d] [dx 0] [w 0]) ([e (in-list l)]
                                               [i (in-naturals)])
      (define-values (ep ew eh)
        (die-cut-text/size e #:font (if (even? i)
                                        (make-font #:family 'modern #:weight 'bold)
                                        (make-font))))
      (values (combine p (move-x ep dx))
              (+ dx ew)
              eh))]))

(define (stack . gens)
  (let loop ([gens gens] [dy 0])
    (if (null? gens)
        empty-pict3d
        (let-values ([(p w h) (text-line (car gens))])
          (combine (move-y p (- (+ dy h)))
                   (loop (cdr gens)
                         (+ dy h)))))))

(define old-cites-text
  (freeze
   (set-color
    (scale (stack "Kohlbecker et al. [LFP'86]"
                  "Bawden & Rees [LFP'88]"
                  "Clinger & Rees [POPL'91]"
                  "Dybvig et al. [LSC'93]"
                  "Waddell & Dybvig [POPL'99]")
           0.2)
    (rgba 1 1 0.8 1))))

(define old-forms-text
  (move (freeze
         (set-color
          (scale (stack (list "λ" ", " "let" ",")
                        (list "let-syntax" ", ..."))
                 0.2)
          (rgba 1 1 1 1)))
        (dir 0 -18 0)))

(define (flip-down text key)
  (lambda (ti)
    (define now (timeinfo ti 'now))
    (define ft (- now (timeinfo ti key)))
    (rotate-x text
              (+ 30 (max 0 (- 150 (/ ft 3.0)))))))

(define (at-old-cites p)
  (move* (move* p leak-home)
         (dir -38 30 0)))

(define old-cites
  (at-old-cites (flip-down old-cites-text 'old-cites)))
(define old-forms
  (at-old-cites (flip-down old-forms-text 'old-forms)))

(define new-cites-text
  (freeze
   (set-emitted
    (scale (stack "[ICFP'02]"
                  "[JFP'12]"
                  "[GPCE'13]")
           0.1)
    (emitted 1 1 0.8 1))))


(define new-forms-text
  (move
   (freeze
    (set-emitted
     (scale (stack (list "module" ", " "class" ",")
                   "mutual recursion,"
                   "...")
            0.1)
     (emitted 1 1 1 1)))
   (dir 0 -5 0)))

(define (at-new-cites p)
  (move* (move* p
                leak-home)
         (dir 6 0 0)))

(define new-cites
  (at-new-cites (flip-down new-cites-text 'new-cites)))
(define new-forms
  (at-new-cites (flip-down new-forms-text 'new-forms)))

(define (make-unfrozen-identifier txt colors)
  (combine (move-x (move-y (set-color (die-cut-text txt #:font tt-font #:expected-scale 10 #:depth 0)
                                      (rgba 1 1 1 1))
                           3)
                   -2)
           (for/list ([c (in-list colors)]
                      [i (in-naturals)])
             (define h (/ 6 (length colors)))
             (move-y
              (set-color (quad (pos -3 0 -1) (pos -3 h -1) (pos 3 h -1) (pos 3 0 -1)
                               #:back? #t)
                         c)
              (- (* i h) 3)))))

(define (make-identifier txt colors)
  (move-z (freeze (make-unfrozen-identifier txt colors))
          -0.5))

(define quantum
  (let* ([s (lambda (p) p)]
         [x (s (make-identifier "x" null))]
         [y (s (make-identifier "y" null))]
         [z (s (make-identifier "z" null))]
         [white-x (freeze x)]
         [blue-color (rgba 0 0 1.0 1.0)]
         [blue-x (freeze (set-color x blue-color))]
         [white-y (freeze y)]
         [red-color (rgba 0.8 0 0 1.0)]         
         [red-y (freeze (set-color y red-color))]
         [white-z (freeze z)])
    (define all-q
      (lambda (ti)
        (define now (timeinfo ti 'now))
        (define history? (syncinfo ti 'quantum-history?))
        (define (recolor past base dt)
          (define s (min 1.0 (/ dt 250.0)))
          (if (or history? (s . < . 1))
              (combine* base
                        (move-y (move-x (scale past (- 1 (* s 0.5))) (* s 3))
                                (* s 3)))
              base))
        (define (rename from to dt)
          (define a (min (/ dt 5)
                         (if history?
                             +inf.0
                             180.0)))
          (combine
           (rotate-y from a)
           (rotate-y to (+ 180 a))))
        (define-syntax quantum-chain
          (syntax-rules ()
            [(_ e) e]
            [(_ e [key op to] c ...)
             (let ([q e]
                   [t (timeinfo ti key)])
               (if (t . < . now)
                   (quantum-chain (op q to (- now t)) c ...)
                   q))]))                          
        (quantum-chain white-x
                       ['color-x recolor blue-x]
                       ['rename-x rename white-y]
                       ['color-y recolor red-y]
                       ['rename-y rename white-z])))
    (move* all-q leak-home)))
  
(define identifiers (vector
                     (make-identifier "x" (list (rgba 0 0 1 1)))
                     (make-identifier "y" (list (rgba 1 0 0 1) (rgba 0 1 0 1)))
                     (make-identifier "z" (list (rgba 1 0 1 1) (rgba 0 1 0 1) (rgba 1 1 0 1)))))

(define house (freeze
               (combine
                (cube (pos 0 0 6) 6)
                (let ([t (triangle (pos -6 -6 12)
                                   (pos 6 -6 12)
                                   (pos 0 0 18))])
                  (for/list ([i (in-range 4)])
                    (rotate-z t (* i 90)))))))

(define normal-item (freeze (sphere (pos 0 0 -3) 2)))

(define plain-alert (pipe (pos 0 0 -8)
                          (dir 6 6 0.1)
                          #:bottom-radii (interval 0.8 1)))
(define bright-alert (freeze (set-emitted
                              plain-alert
                              (emitted 1 1 0 1.0))))

(define (make-alert n)
  (cond
   [(zero? n) empty-pict3d]
   [(n . < . 1) (set-color plain-alert (rgba 1 1 0 n))]
   [else bright-alert]))

(define (pipeline from to col #:leak? [leak? #f])
  (define len (dir-dist (dir- to from)))
  (define pipe
    (freeze
     (set-color (rectangle (pos -3 0 0)
                           (pos 3 (- len) -6))
                (rgba (rgba-red col) (rgba-green col) (rgba-blue col) 0.5))))
  (define rotated-leak-home
    ;; Find relative location of `leak-home`:
    (and leak?
         (let ([d (dir- leak-home from)])
           (let-values ([(yaw pitch) (dir->angles d)])
             (dir-scale (angles->dir (+ yaw (+ 90 (dir-angle (dir- to from)))) pitch)
                        (dir-dist d))))))
  (lambda (ti)
    (define now (timeinfo ti 'now))
    (define t (- now (timeinfo ti 'dissolve)))
    (define use-id? ((timeinfo ti 'identifiers) . < . now))
    (define houses? ((timeinfo ti 'houses) . < . now))    
    (define leak-t (if use-id?
                       0
                       (- now (timeinfo ti 'leak))))
    (define unleak-t (if use-id?
                         0
                         (- now (timeinfo ti 'unleak))))
    (define N (min (inexact->exact (floor (/ len 40)))
                   (add1 (floor (/ t br-ms)))))
    (define dl (/ len N))
    (define (get-dy t) (* dl (/ (modulo (floor t) br-ms) br-ms)))
    (define dy (get-dy t))
    (define leak-i (and leak?
                        (positive? leak-t)
                        (modulo (quotient (floor t) br-ms) N)))
    (define rot-i (quotient (floor t) br-ms))
    (move (rotate-z
           (combine
            pipe
            (for/list ([i (in-range N)])
              (define (idy i dy) (- 0 (* i dl) dy))
              (define item
                (if use-id?
                    (vector-ref identifiers
                                (modulo (inexact->exact (- rot-i i))
                                        (vector-length identifiers)))
                    normal-item))
              (cond
                [(and leak-i
                      (= leak-i i))
                 (define orig-leak-i (modulo (quotient (floor (- t leak-t)) br-ms) N))
                 (define ldy (get-dy (- t leak-t)))
                 (define orig-dy (idy orig-leak-i ldy))
                 (define alert-item (combine item (make-alert
                                                   (max 0
                                                        (min 1 (/ (if (positive? unleak-t)
                                                                      (- br-ms unleak-t)
                                                                      (- leak-t (/ br-ms 2)))
                                                                  (* 2 br-ms)))))))
                 (move (move-y alert-item orig-dy) ; start at position at time of leak
                       ;; Move toward `leak-home`:
                       (dir-scale (dir- rotated-leak-home (dir 0 orig-dy 0))
                                  (/ (min (sqrt leak-t) 50) 50)))]
                [else
                 (move-y item (idy i dy))]))
            (if houses?
                (for/list ([i (in-range N)])
                  (move-y (combine (move-x house -10)
                                   #;(move-x house 10))
                          (- (* -0.3 len) (* i 0.6 dl))))
                null))
           (+ 90 (dir-angle (dir- to from))))
          from)))

(define (dir-angle d)
  (define-values (yaw pitch) (dir->angles d))
  yaw)

(define pipe-to-tr (pipeline init-home tr-home tr-color #:leak? #t))
(define pipe-to-contracts (pipeline tr-home contracts-home contract-color))

(define pipe-to-untyped (pipeline contracts-home untyped-home untyped-color))
(define pipe-tr-to-untyped (pipeline tr-home untyped-home untyped-color))

(define pipe-to-racket (pipeline untyped-home racket-home untyped-color))
(define pipe2-to-racket (pipeline untyped2-home racket-home untyped-color))

(define pipe-to-another-racket (pipeline tr-home another-racket-home untyped-color))
(define pipe2-to-another-racket (pipeline contracts-home another-racket-home untyped-color))

(define all-extra (combine* extra-program
                            extra-factory
                            algol-program
                            algol-program2
                            algol-factory
                            (pipeline extra-factory-home extra-program-home extra-color)
                            (pipeline extra-program-home tr-home tr-color)
                            (pipeline algol-program-home algol-factory-home algol-color)
                            (pipeline algol-factory-home algol-program2-home untyped-color)
                            (pipeline algol-program2-home racket-home untyped-color)))

(define between-factories (dir-scale (dir+ another-racket-home contracts-home) 0.5))

(define yard-cover-home (dir+ between-factories (dir 0 0 2)))
(define short-yard-cover-delta (dir 5 -5 14))

(define yard-cover (move (scale cover 1/20)
                         yard-cover-home))
  
(define inside-init-home (dir+ init-home (dir -5 0 10.5)))
(define tiny-c (freeze (sphere origin 0.15)))

(define (flutter id-pos salt)
  
  (lambda (ti)
    (define t (timeinfo ti 'now))
    (define use-id? ((timeinfo ti 'identifiers) . < . t))
    (define item (if use-id?
                     (scale (vector-ref identifiers id-pos) 0.15)
                     tiny-c))
    (cond
      [#f
       ;; fluttering disabled
       item]
      [else
       (define st (* (* (sin salt) 0.25) (/ t 100)))
       (move-z (move-y (move-x item (* 0.5 (sin st)))
                       (* 0.5 (sin (* 1.3 st))))
               (* 0.1 (sin (* 1.7 st))))])))

(define inside-init
  (move* (combine* (set-color (cube origin 2 #:inside? #t)
                              (rgba 0.5 0.5 0.5 1))
                   (move*-y (move*-z (flutter 0 2) -1.25) -1)
                   (move*-y (move*-z (flutter 1 1.3) -0.75) 0.5)
                   (move*-x (move*-z (flutter 2 3) -1.25) -1))
         inside-init-home))

(struct bldg (factory? home color connects))
               
(define (city bldgs)
  (combine*
   (for/list ([p (in-hash-values bldgs)])
     (cons
      (if (bldg-factory? p)
          (move (factory empty-pict3d) (pos- (bldg-home p) origin))
          (move ((make-program empty-pict3d) 0) (pos- (bldg-home p) origin)))
      (for/list ([d-name (in-list (bldg-connects p))])
        (define d (hash-ref bldgs d-name))
        (pipeline (pos- (bldg-home p) origin) (pos- (bldg-home d) origin) (bldg-color d)))))))

(define cities
  (combine*
   (move* (city (hash 'a (bldg #t (pos 0 0 0) (rgba 1 0 0 1) '(e g))
                      'b (bldg #f (pos 100 50 0) (rgba 0 0 1 1) '(a))
                      'c (bldg #t (pos -80 50 0) (rgba 0 1 0 1) '(b a))
                      'd (bldg #f (pos -30 -50 0) (rgba 0 1 0 1) '(a))
                      'e (bldg #f (pos 120 0 0) (rgba 1 0 0 1) '(f))
                      'f (bldg #t (pos 120 -80 0) (rgba 1 0 0 1) '(g))
                      'g (bldg #f (pos 60 -120 0) (rgba 0 1 0 1) '())
                      'h (bldg #t (pos -60 -110 0) (rgba 0 0 1 1) '(g d))))
          (dir 500 500 0))
   (move* (city (hash 'a (bldg #t (pos 0 0 0) (rgba 1 0 0 1) '(e g))
                      'b (bldg #f (pos -100 0 0) (rgba 0 0 1 1) '(a))
                      'c (bldg #t (pos -80 50 0) (rgba 0 1 0 1) '(b a))
                      'd (bldg #f (pos -30 -50 0) (rgba 0 1 0 1) '(a))
                      'e (bldg #f (pos 100 -30 0) (rgba 1 0 0 1) '(f))
                      'f (bldg #t (pos 120 -80 0) (rgba 1 0 0 1) '(g))
                      'g (bldg #f (pos 60 -120 0) (rgba 0 0 1 1) '())
                      'h (bldg #t (pos -60 -110 0) (rgba 0 0 1 1) '(g d))))
          (dir -500 300 0))
   (move* (city (hash 'a (bldg #t (pos 0 0 0) (rgba 1 0 1 1) '(e g))
                      'b (bldg #f (pos 100 50 0) (rgba 0 0 1 1) '(a))
                      'c (bldg #t (pos 0 50 0) (rgba 0 1 0 1) '(b a))
                      'd (bldg #f (pos -30 -50 0) (rgba 0 1 0 1) '(a))
                      'e (bldg #f (pos 120 0 0) (rgba 1 0 0 1) '(f))
                      'f (bldg #t (pos 120 -80 0) (rgba 1 0 0 1) '())
                      'g (bldg #f (pos -80 0 0) (rgba 0 1 0 1) '())
                      'h (bldg #t (pos -60 -110 0) (rgba 0 0 1 1) '(g d))))
          (dir -200 -400 0))))


(define world (list))

(struct camera-at (pos toward))
(struct show (p))
(struct hide (p))
(struct input (key))
(struct synch (key))
(struct synch-as synch (val))
(struct dialate (p))

(define in-factory-front (pos 0 -70 40))

(define (as-immediate . ps)
  (let loop ([ps ps])
    (cond
     [(null? ps) null]
     [(list? (car ps))
      (loop (append (car ps) (cdr ps)))]
     [(or (input? (car ps))
          (number? (car ps)))
      (loop (cdr ps))]
     [else
      (cons (car ps) (loop (cdr ps)))])))

(define (camera-at-factory home)
  (camera-at (pos+ in-factory-front home) (pos+ origin home)))

(define (camera-at-sky)
  (camera-at (pos+ (pos 0 -20 200) untyped-home) (pos+ (pos 0 30 0) untyped-home)))
(define (camera-at-low-sky)
  (camera-at (pos+ (pos 0 -20 100) untyped-home) (pos+ (pos 0 30 0) untyped-home)))
(define (camera-at-above-init)
  (camera-at (pos+ (pos 30 -30 100) init-home) (pos+ (pos 30 0 0) init-home)))
(define (camera-at-space)
  (camera-at (pos 0 -20 700) origin))

(define (make-movie-path short?)
  (define tr-path (list
                   (show init-program-solid)
                   (if short?
                       null
                       (list
                        (show init-cover)
                        (show ground)))
                   (camera-at (pos+ (pos 0 -20 5) init-home) (pos 0 150 5))
                   (input " ")
                   (if short?
                       null
                       (list
                        500
                        (camera-at (pos+ (pos 0 -35 10) init-home) (pos 0 150 5))
                        200
                        ;(input " ")
                        (synch 'uncover)
                        200
                        (camera-at (pos+ (pos 0 -35 10) init-home) (pos 0 150 5))
                        ;(input " ")
                        500
                        (camera-at (pos+ (pos 0 -1 10) cover-home) (pos+ origin cover-home))
                        (hide ground)
                        (hide init-cover)
                        200
                        (camera-at (pos+ (pos 0 -1 -5) cover-home) (pos+ (pos 0 0 -100) cover-home))
                        500))
                   (hide init-program-solid)
                   (show init-program)
                   (show pipe-to-tr)
                   (synch 'dissolve)
                   (if short?
                       null
                       (list
                        (camera-at (pos+ (pos 0 0 -10) cover-home) origin)
                        (input " ")))
                   (show tr-factory)
                   500
                   (camera-at (pos+ (pos 0 -20 5) init-home) (pos 0 150 5))
                   (input " ")
                   1500
                   (camera-at (pos 0 -140 80) origin)
                   1500
                   (camera-at-factory tr-home)))

  (define ensemble-path
    (list (input " ")
          (show pipe-to-contracts)
          (show contracts-factory)
          1000
          (camera-at-factory contracts-home)
          (input " ")
          (show pipe-to-untyped)
          (show pipe-tr-to-untyped)
          (show untyped-program-solid)
          1000
          (camera-at (pos+ (pos 0 -40 30) untyped-home) (pos+ (pos 0 0 0) untyped-home))
          (input " ")
          (hide untyped-program-solid)
          (synch 'untyped-dissolve)
          (show untyped-program)
          (show pipe-to-racket)
          (show racket-factory)
          1000
          (camera-at (pos+ in-factory-front racket-home) (pos+ origin racket-home))
          (input " ")
          (show pipe2-to-racket)
          (show untyped2-program)
          1000
          (camera-at-sky)))

  (define meta-path-shows
    (list
     (show another-racket-factory)
     (show pipe-to-another-racket)
     (show pipe2-to-another-racket)))
          
  (define meta-path
    (list (input " ")
          1500
          (camera-at-factory tr-home)
          (input " ")
          (synch 'dissolve-factory)
          meta-path-shows
          1000
          (camera-at (pos+ (pos+ in-factory-front tr-home) (dir 0 0 20)) (pos+ origin another-racket-home))
          (input " ")
          1000
          (camera-at-sky)))

  (define leak-path
    (list (input " ")
          (synch 'leak)))

  (define quantum-path
    (list (hide pipe-to-tr)
          (show quantum)
          (input " ")
          (synch 'color-x)
          (input " ")
          (synch 'rename-x)
          (input " ")
          (synch 'color-y)
          (input " ")
          (synch 'rename-y)
          (input " ")
          (synch-as 'quantum-history? #t)
          (input " ")
          (show old-forms)
          (synch 'old-forms)
          (input " ")
          (show new-cites)
          (synch 'new-cites)
          (input " ")
          (show new-forms)
          (synch 'new-forms)
          (input " ")
          (hide quantum)
          (hide old-cites)
          (hide new-cites)
          (show pipe-to-tr)))

  (define question-path
    (append
     (list (input " ")
           (synch 'unleak)
           1000
           (camera-at (pos+ (pos 0 -5 10) leak-home) (pos+ origin leak-home))
           (input " ")
           (synch 'question)
           (show questions)
           (input " ")
           (synch 'old-cites)
           (show old-cites)           
           (input " ")
           (synch 'question-done))
     quantum-path
     (list (input " ")
           (hide old-forms)
           (hide new-forms)
           500
           (camera-at-above-init)
           (input " "))))

  (define identifier-path
    (list (show inside-init)
          1000
          (camera-at (pos+ (pos 0 0 1.5) inside-init-home) (pos+ (pos 0 0.2 0) inside-init-home))
          (input " ")
          (synch 'identifiers)
          meta-path-shows
          (input " ")
          500
          (camera-at-above-init)
          500
          (camera-at-low-sky)))

  (define topple-path
    (list
     (camera-at (pos 0 -280 30) origin)
     (show topple)
     (input " ")
     (synch 'topple)
     (input " ")
     500
     (camera-at (pos 0 -1 100) origin)
     100
     (camera-at (pos 0 -1 100) origin)
     500
     (camera-at (pos 0 -1 -20) (pos 0 0 -100))
     (hide topple)))

  ;; Movie path:
  (list
   tr-path
   ensemble-path
   #;
   (if short?
   null
   meta-path)
   (input " ")
   (show all-extra)
   (if short?
       null
       (list
        leak-path
        question-path
        identifier-path
        (list (show cities)
              (input " ")
              500
              (camera-at-space)
              (input " ")
              500
              (camera-at-sky)
              (hide cities))
        (list (input " ")
              (synch 'houses)
              (input " ")
              500
              (camera-at-factory between-factories))
        (show yard-cover)
        (input " ")
        500
        (camera-at (pos+ (pos 0 -3 6) yard-cover-home) (pos+ origin yard-cover-home))))))
                    

(define ((combine-at t) . l)
  (let loop ([l l])
    (cond
      [(null? l) empty-pict3d]
      [(list? (car l))
       (loop (append (car l) (cdr l)))]
      [(procedure? (car l))
       (loop (cons ((car l) t) (cdr l)))]
      [else
       (combine (car l) (loop (cdr l)))])))

(struct state (start input speed from-real-t from-scaled-t paused?))
(struct received (key time))

(define gl-config (new gl-config%))
(send gl-config set-legacy? #f)
(send gl-config set-hires-mode #f)
(send gl-config set-multisample-size 4)
(send gl-config set-sync-swap #t)

(define (scale-time st t-in)
  (define speed (state-speed st))
  (+ (* speed (- t-in (or (state-from-real-t st) t-in)))
     (or (state-from-scaled-t st) t-in)))

(define (time->pict path st t-in)
  (define t (scale-time st t-in))
  (define s (or (state-start st) t))
  (let loop ([path path]
             [clock s]
             [ti (hash)]
             [dt #f]
             [prev-camera #f]
             [in (state-input st)]
             [items null])
    (cond
      [(null? path)
       ((combine-at (hash-set ti 'now t))
        (sunlight (dir -0.2 0 -1))
        (sunlight (dir -0.2 10 -1) (emitted "azure" 0.4))
        world
        (if prev-camera
            (cons
             (basis 'camera
                    (point-at (camera-at-pos prev-camera)
                              (camera-at-toward prev-camera)
                              #:up +z))
             items)
            items))]
      [(list? (car path))
       (loop (append (car path) (cdr path)) clock ti dt prev-camera in items)]
      [else
       (define p (car path))
       (cond
         [(input? p)
          (cond
            [(and clock (pair? in))
             (if (equal? (received-key (car in)) (input-key p))
                 (loop (cdr path) (max clock (received-time (car in)))
                       ti dt prev-camera (cdr in) items)
                 (loop path clock ti dt prev-camera (cdr in) items))]
            [else
             (loop (cdr path) #f ti #f prev-camera in items)])]
         [(camera-at? p)
          (cond
            [dt
             (define zoom-dt (- 0.5 (* 0.5 (cos (* dt pi)))))
             (loop (cdr path)
                   clock
                   ti
                   dt
                   #f
                   in
                   (cons
                    (basis 'camera 
                           (point-at (pos+ (camera-at-pos prev-camera)
                                           (dir-scale (pos- (camera-at-pos p) (camera-at-pos prev-camera)) zoom-dt))
                                     (pos+ (camera-at-toward prev-camera)
                                           (dir-scale (pos- (camera-at-toward p) (camera-at-toward prev-camera)) zoom-dt))
                                     #:up +z))
                    items))]
            [clock
             (loop (cdr path) clock ti dt p in items)]
            [else
             (loop (cdr path) clock ti dt prev-camera in items)])]
         [(show? p)
          (loop (cdr path) clock ti dt prev-camera in
                (if clock
                    (cons (show-p p) items)
                    items))]
         [(hide? p)
          (loop (cdr path) clock ti dt prev-camera in
                (if clock
                    (remove (hide-p p) items)
                    items))]
         [(synch? p)
          (define val (if (synch-as? p)
                          (and clock (synch-as-val p))
                          clock))
          (loop (cdr path) clock (hash-set ti (synch-key p) val) dt prev-camera in items)]
         [(integer? p)
          (define ms (and clock (- t clock)))
          (if ms
              (loop (cdr path)
                    (if (ms . > . p)
                        (+ clock p)
                        #f)
                    ti
                    (if (ms . < . p)
                        (/ ms p)
                        #f)
                    prev-camera
                    in
                    items)
              (loop (cdr path) #f ti #f prev-camera in items))])])))

(define (save-bitmaps path)
  (define full-path path)
  (define dir "/tmp/slides")
  (delete-directory/files dir #:must-exist? #f)
  (make-directory dir)
  (let loop ([path path] [t 0] [st (state 0 null 1 0 0 #f)] [n 0])
    (define (gen t)
      (send (pict3d->bitmap (time->pict full-path st t) 1024 768)
            save-file
            (build-path dir (format "slide-~a~a.png" (if (n . < . 10) "0" "") n))
            'png))
    (if (null? path)
        (gen t)
        (let ([p (car path)])
          (cond
            [(integer? p)
             (gen (+ t (/ p 2.0)))
             (loop (cdr path) (+ t p) st (add1 n))]
            [(input? p)
             (gen t)
             (loop (cdr path)
                   (+ t 510)
                   (struct-copy state st [input (append (state-input st) (list (received (input-key p) (+ t 500))))])
                   (add1 n))]
            [else (loop (cdr path) t st n)])))))

(define (movie #:mode [mode 'movie]
               #:short? [short-film? #f]
               #:speed [speed 1])
  (define path
    (make-movie-path short-film?))
  (case mode
    [(movie)
     (big-bang3d (state #f null speed #f #f #f)
                 #:display-mode 'hide-menu-bar
                 #:gl-config gl-config
                 #:on-frame (lambda (st n t) (if (state-start st)
                                            st
                                            (struct-copy state st
                                                         [start t]
                                                         [from-real-t t]
                                                         [from-scaled-t t])))
                 #:on-mouse (lambda (st n t x y e) (if (or (equal? e "left-down")
                                                      (equal? e "middle-down")
                                                      (equal? e "right-down"))
                                                  (unpause st)
                                                  st))
                 #:on-key (lambda (pre-st n t k)
                            (define st (unpause pre-st))
                            (hide-cursor-until-moved)
                            (if (state-start st)
                                (case k
                                  [("escape") (exit)]
                                  [("p") (struct-copy state st [paused? (not (state-paused? pre-st))])]
                                  [("1")
                                   (state t null (state-speed st) t t #f)]
                                  [("right")
                                   (struct-copy state st
                                                [start (- (state-start st) 1000)]
                                                [input (for/list ([i (in-list (state-input st))])
                                                         (struct-copy received i [time (- (received-time i) 1000)]))])]
                                  [("left")
                                   (struct-copy state st
                                                [start (+ (state-start st) 1000)]
                                                [input (if (pair? (state-input st))
                                                           (cdr (state-input st))
                                                           null)])]
                                  [else
                                   (struct-copy state st [input (append
                                                                 (state-input st)
                                                                 (list (received k (scale-time st t))))])])
                                st))
                 #:on-draw (lambda (st n t)
                             (time->pict path st t))
                 #:pause-state? (lambda (st n t) (state-paused? st)))]
    [(bitmaps)
     (save-bitmaps path)]))

(define (unpause st)
  (if (state-paused? st)
      (struct-copy state st [paused? #f])
      st))

(module+ main
  (require racket/cmdline)
  
  (define short? #f)
  (define mode 'movie)
  (define speed 1)
  
  (command-line
   #:once-each
   [("--short" "-s") "Short mode"
    (set! short? #t)]
   [("--bitmaps") "Generate bitmaps only"
    (set! mode 'bitmaps)]
   [("--speed") s "Set speed"
    (set! speed (string->number s))
    (unless (real? speed)
      (error 'movie "given speed is not a real number"))])
  
  (movie #:short? short?
         #:mode mode
         #:speed speed))
