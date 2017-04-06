#lang slideshow
(require slideshow/code
         slideshow/balloon
         "config.rkt"
         "utils.rkt"
         "../meta-time/in-file.rkt"
         "../tower/lisp.ss"
         "lesson.rkt")

(provide hash-lang-slides)

(define (module-lang-slides)
  (define module-lang-title "A Module's Language")
  (define (module-lang-slide #:initial-import? [initial-import? #f]
                             #:parens? [parens? initial-import?])
    (slide
     #:title module-lang-title
     (let ([racket/base (code racket/base)])
       (let* ([p
               (vc-append
                (* 2 gap-size)
                (mk-file #:name "grocery"
                         #:suffix "rkt"
                         (code (require "gui.rkt")
                               (define shop ....)))
                ((if parens? values ghost)
                 (t "="))
                ((if parens? values ghost)
                 (code (module grocery #,racket/base
                         (require "gui.rkt")
                         (define shop ....)))))]
              [p (if initial-import?
                     (pin-balloon (wrap-balloon (para #:fill? #f
                                                      #:width (* client-w 1/2)
                                                      "initial import"
                                                      "= language")
                                                's 0 gap-size)
                                  p
                                  racket/base ct-find)
                     p)])
         p))))
  (module-lang-slide)
  (module-lang-slide #:parens? #t)
  (module-lang-slide #:initial-import? #t)
  
  (define lang/htdp-beginner (code lang/htdp-beginner))
  
  (define (make-beginner check-empty)
    (code (module milk #,lang/htdp-beginner
            (define (got-milk? ls)
              (cond
               [#,check-empty false]
               [(cons? ls) ....])))))
    
  (slide
   #:title module-lang-title
   'alts
   (list
    (list (para (make-beginner (code (empty? ls)))))
    (list (para (pin-balloon (wrap-balloon (para #:fill? #f
                                                 "Smaller than" (code racket/base))
                                           's 0 gap-size)
                             (make-beginner (code (empty? ls)))
                             lang/htdp-beginner ct-find)))
    (list (para (make-beginner (code (code:line empty? ls)))))
    (let ([prog (make-beginner (code (code:line #,(enpink (code empty?)) ls)))])
      (list (para prog)
            (blank)
            (error-para "empty?: expected a function call, but there is no open parenthesis before this function")))))
  
  (define reader-title "Reading Numbers")
  
  (define (move-code 0p1)
    (code
     (define (move n)
       (+ n #,0p1))
     code:blank
     (check-expect
      (move (move (move (move (move 0.5)))))
      1.0)))

  (slide
   #:title reader-title
   'alts
   (let* ([0p1 (code 0.1)]
          [p (code (module walk lang/htdp-beginner
                     code:blank
                     #,(move-code 0p1)))])
     (list
      (list p)
      (list (pin-balloon (wrap-balloon (para #:fill? #f
                                             "Does not mean" (code 1/10) "!")
                                       'nw (- gap-size) 0)
                         p
                         0p1 rc-find)))))
  
  (slide
   #:title reader-title
   (mk-file (code
             (eval-when (:compile-toplevel :load-toplevel)
               (setq *readtable* exact-rationals))
             code:blank
             (defun (move n)
               (+ n 0.1)))
            #:name "walk"
            #:suffix "lsp"))
  
  (slide
   #:title (hbl-append (titlet "Using ") (tt "#lang"))
   'alts
   (let* ([htdp/bsl (code htdp/bsl)]
          [p (mk-file (code #,(tt "#lang") #,htdp/bsl
                            code:blank
                            #,(move-code (code 0.1)))
                      #:name "walk"
                      #:suffix "rkt")])
     (list
      (list p)
      (list (pin-balloon (wrap-balloon (para #:fill? #f
                                             #:width (* client-w 1/2)
                                             "Converts remaining characters to"
                                             (code (module ....)))
                                       's 0 gap-size)
                         p
                         htdp/bsl ct-find)))))
  
  (slide (t "[demo in DrRacket]"))
  
  (define (nonterm s) (colorize (t (format "\u2329~a\u232A" s)) "blue"))
  (slide
   #:title "The Core Racket Grammar"
   (add-cite
    (table 3
           (list (nonterm "module")
                 (tt "::=")
                 (code #,(tt "#lang") #,(nonterm "module-name") #,(nonterm "any")))
           cc-superimpose cc-superimpose
           gap-size gap-size)
    "Flatt et al. [ICFP'09]"))
  
  (void))

(define (maybe-add-cite cite p)
  (if cite (add-cite p cite #:page titleless-page) p))

(define (lessons-slide mks
                       #:inevitable? [inevitable? #f]
                       #:incidental? [incidental? inevitable?])
  (for ([mk (in-list mks)])
    (slide
     #:title "Module Parsing Pipeline"
     #:layout 'tall
     (maybe-add-cite
      (and incidental?
           (not inevitable?)
           "Rafkind and Flatt [GPCE'12]")
      (vc-append
       gap-size
       (blank (* 4 gap-size))
       (mk)
       (blank (* 5 gap-size))
       (ht-append (* 1 gap-size)
                  ((if incidental? values ghost)
                   (incidental #:width (* client-w 1/4) "Reader vs. expander details"))
                  ((if inevitable? values ghost)
                   (inevitable (vl-append
                                (current-line-sep)
                                (item #:fill? #f "Explicit language declaration")
                                (item #:fill? #f "Most happens in middle layer"))))))))))

(define (lessons-slides)
  (define (last) (make-pipeline #:reader? #t))
  (lessons-slide (list (lambda () (make-pipeline))
                       (lambda () (make-pipeline #:lang? #t))
                       (lambda () (make-pipeline #:mod? #t))
                       (lambda () (make-pipeline #:req? #t))
                       last))
  (lessons-slide (list last)
                 #:incidental? #t)
  (lessons-slide (list last)
                 #:inevitable? #t))
  
(define (hash-lang-slides)
  (module-lang-slides)
  (lessons-slides))

(module+ main
  (hash-lang-slides))
