#lang slideshow
(require slideshow/code
         slideshow/play
         slideshow/flash
         (only-in texpict/mrpict record))

(provide expand-slides
         as-file*)

(define (ptt s) (colorize (tt s) (current-base-color)))
(define (pstr s) (colorize (tt s) (current-literal-color)))
(define (rec-tt s) (inset (scale (tt s) 0.8) 5))
(define (rec-sub r) (inset r 5))
(define trans-arrow (colorize (arrow (* 2 gap-size) (* pi -1/4)) "forestgreen"))

(define (as-file* p)
  (as-file #f (inset p 5 10)))
    
(define (as-file s content)
  (let ([w (max (* 3 gap-size)
                (+ 6 (pict-width content)))]
        [h (max (* 4 gap-size)
                (+ gap-size (pict-height content)))])
    (let ([icon (file-icon w h "beige")])
      (let ([file (cc-superimpose
                   icon
                   content)])
        (if (not s)
            file
            (inset
             (vc-append
              (current-line-sep)
              file
              s)
             0 0 (/ (min 0 (- (pict-width icon) (pict-width s))) 2) 0))))))

(define (fade-pict* n p1 p2)
  (define-values (n1 n2) (split-phase n))
  (let ([p (fade-pict #:combine lt-superimpose n1 p1 (cellophane p2 n2))])
    (cc-superimpose
     (cellophane (colorize (filled-flash (pict-width p) (pict-height p)) "white")
                 (sin (* pi (fast-middle n))))
     p)))

(define (expand-slide code-n mod-n ast-n)
  (define scribble-orig
    (as-file*
     (vl-append
      (current-code-line-sep)
      (code #,(tt "#lang") #,(fade-pict* code-n (code scribble/base) (code at-exp scheme)))
      (htl-append (tt "@") (code (require scriblib/figure)))
      (hbl-append (tt "@")
                  (code bold)
                  (ptt "{")
                  (colorize (tt "Hi") (current-literal-color))
                  (ptt "}")))))
  (define scribble-mod
    (code
     (module m #,(fade-pict* mod-n (code scribble/base) (code scheme))
       (require scriblib/figure)
       (bold "Hi"))))
  (define scribble-core
    (record
     (rec-sub
      (ht-append
       gap-size
       (vl-append
        (/ gap-size 2)
        (record
         (rec-tt "import")
         (fade-pict* ast-n (rec-tt "scribble/base") (rec-tt "scheme"))
         (rec-tt "scriblib/figure"))
        (cellophane
         (record
          (rec-tt "export")
          (rec-tt "doc"))
         (- 1 ast-n)))
       (let ([app (record
                   (rec-tt "apply")
                   (rec-tt "bold")
                   (rec-tt "(\"Hi\")"))])
         (let ([r (record
                   (rec-tt "define")
                   (rec-tt "doc")
                   (rec-sub (ghost app)))])
           (pin-over (cellophane r (- 1 ast-n))
                     app
                     lt-find
                     app)))))))
      
  (define (shaded p)
    (let ([p (inset p 5)])
      (cc-superimpose
       (colorize (filled-rounded-rectangle (pict-width p) (pict-height p) 10)
                 "lightgray")
       p)))

  (define (backing p)
    (cc-superimpose
     (colorize (filled-rectangle (pict-width p) (pict-height p))
               "beige")
     p))

  (vc-append
   gap-size
   (para #:width client-w #:align 'left 
         (let ([p scribble-orig])
           (refocus (hc-append gap-size p (colorize (it "source") "blue")) p)))
   (vl-append
    gap-size
    (refocus (hc-append gap-size (inset (colorize (it "read") "red") 0 0 0 6) trans-arrow) trans-arrow)
    (vr-append
     gap-size
     (shaded scribble-mod)
     (refocus (hc-append (* gap-size 0.75) trans-arrow (inset (colorize (it "expand") "red") 0 0 0 6)) trans-arrow)))
   (para #:width client-w #:align 'right 
         (let ([p (backing scribble-core)])
           (refocus (hc-append (* 1.5 gap-size) (colorize (it "AST") "blue") p) p)))))

(define (expand-slides)
  (play-n #:name "Expand" expand-slide))

