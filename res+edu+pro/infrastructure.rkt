#lang slideshow
(require racket/runtime-path
         "history.rkt"
         "util.rkt"
         "logo.rkt"
         "asm.rkt"
         "gui.rkt"
         "pkg.rkt")

(provide infrastructure-slides)

(define sqlite-png "logos/sqlite.png")
(define openssl-png "logos/openssl.png")
(define gtk-png "logos/gtk.png")

(define sqlite (bitmap sqlite-png))
(define openssl (bitmap openssl-png))
(define gtk (bitmap gtk-png))

(define plot-png "plot.png")
(define plot-image (scale (bitmap plot-png) 0.75))

(define (infrastructure-slides)
  (as-history
   #:res 0
   #:edu 0

   (slide
    #:title (ca 2004 "Foreign Function Interface" #:who "Barzilay")
    #:name  "Foreign Function Interface"
    (let* ([p (ht-append (* 7 gap-size)
                         (scale logo 1/2)
                         (inset (vc-append (* 2 gap-size)
                                           (scale sqlite 3/4)
                                           (scale openssl 1/3)
                                           (scale gtk 1/2))
                                0 gap-size 0 0))]
           [connect (lambda (p from to-find)
                      (pin-arrow-line gap-size
                                      p
                                      logo to-find
                                      from (shifted lc-find (- gap-size) 0)
                                      #:line-width 3
                                      #:color "gray"))]
           [p (connect p sqlite (shifted rc-find (* 1/2 gap-size) 0))]
           [p (connect p openssl (shifted rc-find (* 1/4 gap-size) (* 1/8 (pict-height logo))))]
           [p (connect p gtk rb-find)])
      p))

   (slide
    #:title (ca 2006 "Native-Code Compiler")
    #:name  "Native-Code Compiler"
    asm-code)

   (slide
    #:title (ca 2010 "GUI Library Rewrite")
    #:name  "GUI Library Rewrite"
    (vc-append gap-size
               gui-win-image
               (inset gui-image
                      0 (* -2 gap-size) 0 (* -2 gap-size))
               gui-gtk-image))

   (slide
    #:title (ca 2012 "Math & Plot" #:who "Toronto")
    #:name "Math & Plot"
    plot-image)

   (slide
    #:title (ca 2014 "Packaging, Distribution, and Build" #:who "McCarthy & Flatt")
    #:name  "Packaging, Distribution, and Build"
    (scale (pkg-icon #:logo-inside? #t) 3))

   (void)))

(module+ main
  (infrastructure-slides))

