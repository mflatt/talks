#lang slideshow

(provide mk-file
         vfactor
         file-background
         lb*-find
         rb*-find
         lt*-find
         ct*-find
         c*t-find)

(define file-background "lightblue")

(define vfactor 1.2)

(define (lb*-find in p)
  (lb-find in p)
  #;
  (let-values ([(x y) (lb-find in p)])
    (values x (- y font-size))))

(define (rb*-find in p)
  (rb-find in p)
  #;
  (let-values ([(x y) (rb-find in p)])
    (values x (- y font-size))))

(define (lt*-find in p)
  (let-values ([(x y) (lt-find in p)])
    (values x (+ y (current-font-size)))))

(define (ct*-find in p)
  (let-values ([(x y) (ct-find in p)])
    (values x (+ y (current-font-size)))))

(define (c*t-find in p)
  (let-values ([(x y) (ct-find in p)])
    (values (+ x (* 1/4 (pict-width p))) y)))

(define mk-file
  (lambda (content #:name [name #f] 
                   #:suffix [suffix "scm"]
                   #:color [kw-color #t]
                   #:content-only? [content-only? #f]
                   #:shape [shape 'mod]
                   [color kw-color] 
                   [vfactor vfactor] 
                   [border-color "black"])
    (define name-box
      (if name
          (inset (scale (if (string? name)
                            (tt (format "~a.~a" name suffix))
                            name)
                        0.8)
                 6 3)
          (blank)))
    (case shape
      [(semi-mod mod)
       (define background
         (colorize (filled-rectangle (+ (pict-width content) (current-font-size))
                                     (+ (pict-height content) (current-font-size)))
                   (if color
                       (if (boolean? color)
                           file-background
                           color)
                       "white")))
       (define c
      (cc-superimpose
       (if content-only?
           background
           (frame background))
       content))
       (define r
         (if name
             (let ([n name-box])
               (define f
                 (if (eq? shape 'mod)
                     (cc-superimpose (frame
                                      (colorize (filled-rectangle (pict-width n)
                                                                  (pict-height n))
                                                "beige"))
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
                                file-background
                                color)
                            #t)
                 content)])
         (if name
             (vr-append name-box
                        f)
             f))])))
