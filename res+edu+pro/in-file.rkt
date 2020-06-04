#lang slideshow
(require "color.rkt")

(provide in-file)

(define in-file
  (lambda (content #:name [name #f] 
                   #:suffix [suffix "rkt"]
                   #:color [kw-color #t]
                   #:content-only? [content-only? #f]
                   #:shape [shape 'mod]
                   [color kw-color] 
                   [border-color "black"])
    (define name-box
      (ghost
       (if name
           (inset (scale (if (string? name)
                             (tt (format "~a.~a" name suffix))
                             name)
                         0.8)
                  6 3)
           (blank))))
    (case shape
      [(semi-mod mod)
       (define background
         (colorize (filled-rectangle (+ (pict-width content) (current-font-size))
                                     (+ (pict-height content) (current-font-size)))
                   (if color
                       (if (boolean? color)
                           module-background
                           color)
                       "white")))
       (define c
         (cc-superimpose
          (if content-only?
              background
              (x-frame background))
          content))
       (define r
         (if name
             (let ([n name-box])
               (define f
                 (if (eq? shape 'mod)
                     (cc-superimpose (x-frame
                                      (colorize (filled-rectangle (pict-width n)
                                                                  (pict-height n))
                                                file-tab-color))
                                     n)
                     n))
               (vr-append (inset f (min 0 (- (pict-width c) (pict-width f))) 0 0 0)
                          c))
             c))
       (if content-only?
           (pin-over (ghost r) c lt-find c)
           r)]
      [(file)
       (let ([f (cc-superimpose
                 (file-icon (+ (pict-width content) (current-font-size))
                            (+ (pict-height content) (current-font-size))
                            (if (and color (boolean? color))
                                module-background
                                color)
                            #t)
                 content)])
         (if name
             (vr-append name-box
                        f)
             f))])))

(define (x-frame p) p)

