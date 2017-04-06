#lang slideshow
(require slideshow/flash
         slideshow/code
         "../../meta-time/logo.rkt")

(provide experience-slides)

(define (dt s)
  (colorize (scale (t s) 0.75) "blue"))
(define (gt s)
  (colorize (t s) "forestgreen"))

(define new-label
  (let* ([new (text "NEW" '(bold . swiss) 12 (/ pi 8))]
         [bg (filled-flash (* 3/2 (pict-width new)) (* 3/2 (pict-height new))
                           10 0.25 (/ pi 8))])
    (cc-superimpose
     (colorize bg "gold")
     new)))

(define check (inset (scale (colorize (bt "✓") "forestgreen") 1.2) 0 0 5 0))

(define (experience-slides)
  (slide
   #:layout 'tall
   ; #:title "Racket Experience"
   (vc-append
    (* 2 gap-size)
    (inset (scale (make-racket-logo #:use-pen? #f) 1/4) 0 gap-size 0 0)
    (hc-append
     (* 2 gap-size)
     (vl-append
      (current-line-sep)
      (para #:fill? #f "Version 2.0 to 6.2" (dt "(2002–2015)"))
      (bt "marks and renames")
      (scale (t "Dybvig et al. [LSC'93], Flatt et al. [JFP'12]") 0.75))
     (colorize (arrow (* 2 gap-size) 0) "forestgreen")
     (inset
      (vl-append
       (current-line-sep)
       (para #:fill? #f "Version 6.3" (dt "(2015)"))
       (hbl-append (bt "set of scopes"))
       new-label)
      (* 1 gap-size) 0 (* 3 gap-size) 0)))
   'next
   (blank)
   'alts
   (list
    (list (blank)
          (vl-append
           gap-size
           (item #:bullet check #:fill? #f "Simpler implementation")
           (blank)
           (item #:bullet check #:fill? #f "Same performance")
           (blank)
           (item #:bullet check #:fill? #f (it "Mostly") "compatible")))
    (list
     (blank)
     (vc-append
      (current-line-sep)
      (para "Fixed macros:")
      (para #:fill? #f
            (vl-append
             gap-size
             (code class code:blank unit code:blank #,(t "..."))
             (code #,(tt "#lang") typed/racket))))
     (blank)
     (blank)
     (para "Adjusted" (gt "~35 of 600") "packages" (dt "(≪1k of 1M LOC)"))))))
  
(module+ main
  (experience-slides))
