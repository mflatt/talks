#lang slideshow
(require slideshow/code
         slideshow/play
         "in-file.rkt"
         "style.rkt")

(provide thesis-slides
         thesis-module)

(define (block n m)
  (blank (* n gap-size) (* m gap-size)))

(define (annote p a)
  (rb-superimpose p (colorize a "forestgreen")))

(define (titem . r)
  (parameterize ([current-main-font  `(bold . ,(current-main-font ))])
    (apply para r)))

(define (self-last p) (use-last p p))

(define (thesis-module #:new? [new? #f]
                       #:enforce? [enforce? new?]
                       #:declare? [declare? enforce?]
                       #:arrows? [arrows? #f]
                       #:name [name "program"]
                       #:wrapping-color [block-color comptime-color]
                       #:split-n [split-n 0]
                       #:split-arrow? [split-arrow? #f])
  (let* ([i-block (block 4 1)]
         [inside (encloud i-block)]
         [inside-hole (encloud i-block #:color file-background)]
         [i1 (launder (ghost inside))]
         [add-inside (lambda (p i)
                       (lt-superimpose
                        p
                        (inset i (* 2 gap-size) (* 1.5 gap-size) 0 0)))]
         [wrapping (add-inside (encloud (block 12 3) #:color block-color)
                               inside-hole)]
         [w1 (add-inside (ghost (launder wrapping))
                         i1)]
         [w2 (ghost (launder wrapping))]
         [extra1 ((if new? values ghost)
                  (encloud (block 11 1) #:color new-color))]
         [ex11 (launder (ghost extra1))]
         [ex12 (launder (ghost extra1))]
         [extra2 ((if new? values ghost)
                  (encloud (block 7 1) #:color new-color))]
         [ex21 (launder (ghost extra2))]
         [ex22 (launder (ghost extra2))]
         [f (mk-file #:name name
                     #:suffix (if declare? "rkt" "scm")
                     #:shape (if declare? 'mod 'file)
                     (vl-append (/ gap-size 2)
                                (cc-superimpose (encloud (block 12 3))
                                                ((if (and enforce? (not new?))
                                                     values
                                                     ghost)
                                                 (frame
                                                  (linewidth
                                                   #f
                                                   (colorize
                                                    (filled-rectangle (* 3 gap-size) gap-size)
                                                    "pink"))
                                                  #:color "red")))
                                (inset ex11
                                       gap-size 0 0 0)
                                (inset
                                 w1
                                 gap-size)
                                ex21
                                (vl-append
                                 (encloud (block 16 2))
                                 (fade-pict split-n
                                            #:combine lt-superimpose
                                            (blank)
                                            (code
                                             code:blank
                                             (module code:blank code:blank
                                               #,ex12
                                               #,(self-last (inset ex22 0 0 5 0)))
                                             (module code:blank code:blank
                                               #,(self-last (inset w2 0 0 5 0))))))))]
         [p (slide-pict f extra1 ex11 ex12 split-n)]
         [p (slide-pict p extra2 ex21 ex22 split-n)]
         [p (slide-pict p wrapping w1 w2 split-n)]
         [p (pin-over p i1 lt-find inside)]
         [p (scale (inset p 0 0 0 (* 2 gap-size))
                   0.8)]
         [a gap-size]
         [w 7]
         [p (if (and arrows? declare?)
                (pin-arrow-line a p
                                f (adj cb-find (- gap-size) 0)
                                p (adj cb-find (- gap-size) 0)
                                #:color runtime-color
                                #:line-width w)
                p)]
         [p (if (and arrows? declare?)
                (pin-arrow-line a p
                                f (adj cb-find gap-size 0)
                                p (adj cb-find gap-size 0)
                                #:color comptime-color
                                #:line-width w)
                p)]
         [p (if (and arrows? new?)
                (pin-arrow-line a p
                                f (adj cb-find (* 3 gap-size) 0)
                                p (adj cb-find (* 3 gap-size) 0)
                                #:color new-color
                                #:line-width w)
                p)]
         [p (if split-arrow?
                (pin-arrow-line (/ a 2) p
                                inside-hole rt-find
                                inside rb-find
                                #:start-angle (* pi 1/4)
                                #:end-angle (* pi 3/4)
                                #:color ref-color
                                #:line-width 3)
                p)])
    p))

(define (thesis-slide #:new? [new? #f]
                      #:enforce? [enforce? new?]
                      #:declare? [declare? enforce?])
  (slide
   (thesis-module #:new? new? #:enforce? enforce? #:declare? declare?
                  #:arrows? #t)
   ((if declare? values ghost)
    (annote (titem "Declare phases")
            (it "modules and imports")))
   ((if enforce? values ghost)
    (annote (titem "Enforce phases through scope")
            (it "syntax objects")))
   ((if new? values ghost)
    (annote (titem "Enable new phases for new tasks")
            (it "submodules")))))

(define (thesis-slides)
  (thesis-slide)
  (thesis-slide #:declare? #t)
  (thesis-slide #:enforce? #t)
  (thesis-slide #:new? #t))

(module+ main
  (thesis-slides))
