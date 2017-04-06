#lang racket
(require pict3d
         pict3d/universe
         (only-in pict pict-width pict-height inset)
         "cover.rkt"
         "pict-to-pict3d.rkt"
         "card.rkt"
         "code.rkt"
         "plan.rkt")

(define mode 'slides)

(struct state (start pos))

(define lights+camera
  (combine (light (pos 0 1 2) (emitted "Thistle"))
           (light (pos 0 -1 -2) (emitted "PowderBlue"))
           #;
           (let ([W 1024]
                 [H 768])
             (set-emitted (quad (pos (* W -1/2)
                                     (* H -1/2)
                                     -10)
                                (pos (* W 1/2)
                                     (* H -1/2)
                                     -10)
                                (pos (* W 1/2)
                                     (* H 1/2)
                                     -10)
                                (pos (* W -1/2)
                                     (* H 1/2)
                                     -10)
                                #:back? #f)
                          (emitted 1.0 1.0 1.0 1.0)))
           (basis 'camera 
                  (point-at (pos 0 -180 30) origin
                            #:up +z))))
 
(define slides-lights+camera
  (combine (basis 'camera 
                  (point-at (pos 0 0 500) origin
                            #:up +y))))
 

;;  (- -90 (* 30 (sin (* pi (/ dt 1000))))))

(define (topple s t)
  (define dt (if s (- t s) 0))
  (define dtx (* (/ (expt (min dt 5000) 3.0)
                    (expt 5000.0 3.0))
                 5000.0))
  (define dh (/ dtx 5000))
  (define rev-cover (rotate-z cover (* 0.9 (/ dtx -5))))
  (define a (+ 60 (* -60 dh)))
  (define spun-cover (rotate-x rev-cover a))
  (combine (move-z (rotate-z spun-cover (- (/ dtx -5)))
                   (* RR (sin (* pi (/ a 180.0)))))
           lights+camera))

(define (on-draw s n t)
  (let ([s (state-start s)])
    (topple s (if s (max s (- t 500)) t))))

(define (center-wrt p sw sh)
  (define w 1024)
  (define h 768)
  (move-y (move-x p
                  (+ (/ w -2) (/ (- w sw) 2)))
          (- (/ h 2) (/ (- h sw) 2))))

(define (center-slide p)
  (center-wrt (pict->pict3d p) (pict-width p) (pict-height p)))

(define code-picts
  (time
   (map center-slide
        (append
         (default-or-example #f)
         (basic-lexical-scope)
         (scopes-example)
         (default-or-example #t)))))

(define code (pict->pict3d (second (scopes-example))))

(define W 1024)
(define H 768)
(define projector-screen (set-emitted (quad (pos (* W -1/2)
                                                 (* H -1/2)
                                                 -10)
                                            (pos (* W 1/2)
                                                 (* H -1/2)
                                                 -10)
                                            (pos (* W 1/2)
                                                 (* H 1/2)
                                                 -10)
                                            (pos (* W -1/2)
                                                 (* H 1/2)
                                                 -10)
                                            #:back? #f)
                                      (emitted 1.0 1.0 1.0 0.5)))

(define (on-draw1 s n t)
  (combine (scale (list-ref code-picts (state-pos s)) 2)
           projector-screen
           slides-lights+camera))

(define plan (script->planneds (default-or-example-script #f)
                               #:adjust-card card->pict3d))

(define (on-plan-draw s n t)
  (define N 1000.0)
  (define dt (/ (min N (- t (or (state-start s) t))) N))
  (define pl (list-ref plan (state-pos s)))
  (combine
   (apply combine (for/list ([p (in-list (planned-cards+positions pl))])
                    (define-values (dx dy) ((cdr p) dt))
                    (center-wrt (move-y (move-x (car p) dx) (- dy))
                                (planned-width pl)
                                (planned-height pl))))
   projector-screen
   slides-lights+camera))

(big-bang3d (state #f 0)
            #:frame-delay #i1000/60
            #:on-key (λ (s n t k)
                       (case k
                         [("esc") (exit)]
                         [(" " "right")
                          (struct-copy state s
                                       [start t]
                                       [pos (min (sub1 (length (if (eq? mode 'slides)
                                                                   code-picts
                                                                   plan)))
                                                 (add1 (state-pos s)))])]
                          [("left")
                           (struct-copy state s [pos (max 0 (sub1 (state-pos s)))])]
                          [else s]))
            #:on-draw (case mode
                        [(cover) on-draw]
                        [(slides) on-draw1]
                        [(plan) on-plan-draw])
            #:on-frame (lambda (s n t)
                         (if (state-start s)
                             s
                             (struct-copy state s [start t]))))
