#lang slideshow
(require slideshow/face
         "skip.rkt")

(provide key-and-mouse-slides)

(define key (scale (bitmap "key-icon.png") 0.5))
(define mouse (scale (bitmap "mouse-icon.png") 0.25))

#;
   (let ([p (bt "is for programmers")]
         [r (scale (bt "Racket") 1.75)]
         [w (pict-width plt-bm)])
     (scale
      (vc-append
       (current-line-sep)
       (hb-append
        gap-size
        r
        (scale plt-bm (/ (- (pict-width p) gap-size (pict-width r))
                         (pict-width plt-bm))))
       p)
      1.2))

(define (face2 mode)
  (case mode
    [(unhappy) (face* 'worried 'plain #t default-face-color 6)]
    [(happier) (face* 'normal 'large #f default-face-color 3)]
    [else (face mode)]))

(define (key-and-mouse-slides* pre-gui post-gui drracket)
  (define (key-slide new? keys? unhappy? drracket?
                     #:n [n (lambda (p n) p)]
                     #:timeout [timeout #f])
    (slide
     #:title "What a Programmer Wants"
     #:timeout timeout
     (blank (* 2 gap-size))
     (vc-append
      gap-size
      (cc-superimpose
       ((if drracket? ghost values)
        (hc-append
         gap-size
         pre-gui
         (cc-superimpose
          ((if (and new? (not keys?)) values ghost)
           (colorize (arrow (* 2 gap-size) 0) "forestgreen"))
          ((if keys? values ghost)
           (hc-append
            gap-size
            (colorize (arrow gap-size 0) "forestgreen")
            (scale
             (vc-append
              (hc-append (n key 0) (n mouse 1) (n mouse 2) (n key 3))
              (hc-append (n mouse 4) (n key 5) (n key 6) (n mouse 7))
              (hc-append (n mouse 8) (n key 9) (n mouse 10) (n key 11)))
             0.75)
            (colorize (arrow gap-size 0) "forestgreen"))))
         ((if new? values ghost)
          post-gui)))
       ((if drracket? values ghost)
        drracket))
      ((if unhappy? values ghost)
       (vc-append
        (scale (face2 (if drracket? 'happier 'unhappy)) 0.5)
        (t "Programmer"))))))

  (begin-or-skip
   (key-slide #f #f #f #f)
   (key-slide #t #f #f #f))
  (for ([i 11])
    (key-slide #t #t #f #f 
               #:n (lambda (p n) ((if (i . >= . n) values ghost) p))
               #:timeout 0.1))
  (begin-or-skip
   (key-slide #t #t #f #f))
  (key-slide #t #t #t #f)
  (key-slide #t #t #t #t))

(define key-pre-gui (scale (bitmap "ppt-bad.png") 0.75))
(define key-post-gui (scale (bitmap "ppt.png") 0.75))
(define key-drracket (scale (bitmap "../scribble/talk/drracket.png") 0.8))

(define (scale-by p q)
  (scale p (min (/ (pict-width q) (pict-width p))
                (/ (pict-height q) (pict-height p)))))

(define (key-and-mouse-slides)
  (key-and-mouse-slides* key-pre-gui
                         key-post-gui
                         (scale-by (bitmap "drracket-slide.png") key-drracket))
  (key-and-mouse-slides* (scale-by (bitmap "../scribble/talk/word.png") key-pre-gui)
                         (scale-by (bitmap "word2.png") key-post-gui)
                         key-drracket))

(module+ main
  (key-and-mouse-slides))

