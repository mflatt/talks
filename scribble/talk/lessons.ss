#lang slideshow
(require slideshow/code
         "expand.ss")

(provide lesson-slides)

(define tech-title "Key PLT Technology")

(define (at p)
  (htl-append (colorize (tt "@") (current-base-color)) p))

(define (braces p)
  (hbl-append (colorize (tt "{") (current-base-color))
              p
              (colorize (tt "}") (current-base-color))))

(define (brackets p)
  (hbl-append (colorize (tt "[") (current-base-color))
              p
              (colorize (tt "]") (current-base-color))))

(define (lt s)
  (colorize (tt s) (current-literal-color)))

(define (prog p) (as-file* p))

(define (tech n s p)
  (ct-superimpose
   (inset (item #:bullet (bt n) s) 0 gap-size 0 0)
   (inset (cc-superimpose p full-page) 0 (- (* 2 gap-size)) 0 0)))

(define (module-slide)
  (slide
   #:title tech-title
   (tech 
    "1." 
    "Everything is a module"
    (scale
     (ht-append
      (* 4 gap-size)
      (prog
       (code #,(tt "#lang") scribble/base
             code:blank
             #,(htl-append (at (code title)) 
                           (braces (lt "Wishing Wells")))
             code:blank
             #,(lt "Make a wish.")))
      (prog 
       (code #,(tt "#lang") scribble/lp
             code:blank
             #,(lt "The princess says hello:")
             code:blank
             #,(htl-append
                (at (code chunk)) 
                (code [<main> 
                       "Hello, kingdom!"])))))
     0.8))))

(define scheme-code (code scheme))
(define boolean?-code (code boolean?))
(define p-code (code p))
(define p-code2 (launder p-code))

(define srcdoc-prog
  (prog
   (code #,(tt "#lang") at-exp #,scheme-code
         (require scribble/srcdoc)
         code:blank
         (provide/doc
          (proc-doc 
           can-wish? (-> [#,p-code person?] #,boolean?-code)
           #,(hbl-append (at (blank))
                         (braces
                          (hbl-append
                           (lt "Returns ")
                           (at (code scheme))
                           (brackets (code #t))
                           (lt " if ")
                           (at (code scheme))
                           (brackets (code #,p-code2))
                           (lt " is a princess."))))))
         code:blank
         (define (can-wish? p) ....))))

(define (compose-slide)
  (slide
   #:title tech-title
   (tech 
    "2." 
    "Use composable layers"
    (scale
     srcdoc-prog
     0.8))))

(define (reflect-slides a1 a2)
  (slide
   #:title tech-title
   (tech 
    "3." 
    "Reflect on lexical scope"
    (a1 (a2 (scale srcdoc-prog 0.8))))))

(define (linker from to)
  (lambda (p)
    (pin-arrow-line (/ gap-size 2)
                    p
                    from cc-find
                    to cc-find
                    #:color "purple"
                    #:line-width 3)))
                    

(define scheme-link (linker boolean?-code scheme-code))
(define p-link (linker p-code2 p-code))

(define (lesson-slides)
  (module-slide)
  (compose-slide)
  (reflect-slides values values)
  (reflect-slides scheme-link values)
  (reflect-slides scheme-link p-link))

(lesson-slides)