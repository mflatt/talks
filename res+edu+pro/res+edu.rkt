#lang slideshow
(require slideshow/play
         "person.rkt"
         "paper.rkt"
         "book.rkt"
         "gear.rkt")

(provide res+edu-slides
         classroom)

(define the-students (delay (make-students)))
(define the-professor (delay (professor)))
(define the-paper (delay (paper #:badge textbook-title)))
(define the-textbook (delay (textbook)))
(define the-gear (delay (gear)))

(define (classroom #:paper? [paper? #t]
                   #:gear? [gear? #f]
                   #:slide [n (if gear? 1 0)]
                   #:paper-slide [paper-n 0]
                   #:gear-copy-n [gear-copy-n 0]
                   #:textbook-fade-n [textbook-fade-n 0]
                   #:gear [a-gear (force the-gear)]
                   #:gear-copy [another-gear a-gear]
                   #:paper [a-paper (force the-paper)]
                   #:professor [prof (force the-professor)]
                   #:students [students (force the-students)])
  (let* ([move-students '(1 2)]
         [ss (map (lambda (s) (scale s 0.75)) students)]
         [g2s (map ghost ss)]
         [g1s (map ghost ss)]
         [g1 (group-students g1s)]
         [g2 (group-students (for/list ([g2 (in-list g2s)]
                                        [i (in-naturals)]
                                        #:when (memq i move-students))
                               g2))]
         [paper-icon ((if paper? values ghost) a-paper)]
         [gear-icon (and gear? (scale a-gear 0.5))]
         [another-gear-icon (and (and gear? (gear-copy-n . > . 0))
                                 (scale another-gear 0.5))]
         [paper-icon+gear (if gear?
                              (refocus (hc-append gap-size
                                                  gear-icon
                                                  paper-icon)
                                       paper-icon)
                              paper-icon)]
         [textbook-icon (cellophane (force the-textbook) (- 1 textbook-fade-n))]
         [g-gear-icon (and gear-icon (ghost (launder gear-icon)))]
         [textbook-icon+gear (if g-gear-icon
                                 (refocus (hc-append gap-size
                                                     textbook-icon
                                                     g-gear-icon)
                                          textbook-icon)
                                 textbook-icon)]
         [p (ht-append
             (* 6 gap-size)
             (vc-append gap-size
                        textbook-icon+gear
                        g1)
             prof
             (vc-append gap-size
                        paper-icon+gear
                        g2))]
         [p (for/fold ([p p]) ([s (in-list ss)]
                               [g1 (in-list g1s)]
                               [g2 (in-list g2s)]
                               [i (in-naturals)])
              (if (memq i move-students)
                  (slide-pict p s g1 g2 (fast-middle n))
                  (pin-over p
                            g1 lt-find
                            s)))]
         [p (if (gear-copy-n . > . 0)
                (let-values ([(sx sy) (lt-find p gear-icon)]
                             [(ex ey) (lt-find p g-gear-icon)])
                  (pin-over p
                            (+ (* ex gear-copy-n) (* sx (- 1 gear-copy-n)))
                            (+ (* ey gear-copy-n) (* sy (- 1 gear-copy-n))
                               (* -100 (sin (* gear-copy-n pi))))
                            another-gear-icon))
                p)]
         [p (if (paper-n . > . 0)
                (let-values ([(px py) (lt-find p paper-icon)]
                             [(tx ty) (lt-find p textbook-icon)])
                  (pin-under p
                             (+ (* tx paper-n) (* px (- 1 paper-n)))
                             (+ (* ty paper-n) (* py (- 1 paper-n))
                                (* -150 (sin (* paper-n pi))))
                             (rt-superimpose (scale paper-icon (- 1 (* 0.4 paper-n)))
                                             (ghost paper-icon))))
                p)])
    p))

(define (res+edu-slides)
  (define name "Research + Education")
  (slide #:name name (classroom #:paper? #f))
  (play-n #:name name (lambda (n) (classroom #:paper-slide (fast-middle n))) #:skip-last? #t)
  (play-n #:name name (lambda (n) (classroom #:slide n)))
  (play-n #:name name (lambda (n) (classroom #:gear? #t #:gear-copy-n (fast-middle n)))))

(module+ main
  (res+edu-slides))
