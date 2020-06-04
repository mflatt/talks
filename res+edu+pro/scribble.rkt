#lang slideshow
(require "history.rkt"
         "doc.rkt"
         "gear.rkt"
         "paper.rkt"
         "pkg.rkt"
         "bar.rkt")

(provide scribble-slides)

(define (doc-slide #:enbadge [enbadge values])
  (slide
   #:title (ca 2008 "Documentation")
   #:name "Documentation"
   (leftward (enbadge (scale doc 1.4)))))

(define (add-lang-badge p)
  (refocus (rb-superimpose
            p
            (inset
             (cc-superimpose
              (scale (gear) 2.2)
              (paper))
             0 0 (* -2 gap-size) (* -2 gap-size)))
           p))

(define (add-lang+pkg-badges p)
  (let ([p (add-lang-badge p)]
        [pkg (scale (pkg-icon) 2)])
    (refocus (rc-superimpose
              p
              (inset pkg 0 (* -1/2 (pict-height pkg)) (* -2 gap-size) 0))
             p)))

(define (add-lang+pkg-badges+department-view p)
  (let ([p (add-lang-badge  (add-lang+pkg-badges p))])
    (refocus (lb-superimpose
              p
              (let ([v (inset (scale (department-view) 0.6) gap-size)])
                (inset (cc-superimpose
                        (colorize (filled-rectangle (pict-width v) (pict-height v)) "white")
                        (frame v))
                       (* -8 gap-size) 0 0 (* -6 gap-size))))
             p)))

(define (scribble-slides)
  (as-history
   #:res 0
   #:edu 0
   (doc-slide))

  (as-history
   #:res 0
   #:edu 1
   (doc-slide))

  (as-history
   #:res 1
   #:edu 0
   (doc-slide #:enbadge add-lang-badge))

  (as-history
   #:res 0
   #:edu 0
   (doc-slide #:enbadge add-lang+pkg-badges)
   (doc-slide #:enbadge add-lang+pkg-badges+department-view)))

(module+ main
  (scribble-slides))
