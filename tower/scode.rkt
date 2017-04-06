#lang slideshow
(require slideshow/code
         racket/draw
         racket/class
         (for-syntax syntax/stx)
         (only-in syntax/parse syntax-parse)
         "utils.ss"
         "color.ss")

(provide scode
         pattern
         template
         colorize-scode)

(define doc-w (* 3/4 client-w))

(define colorize-scode (make-parameter #t))

(define (color-box p color)
  (if (colorize-scode)
      (cc-superimpose
       (colorize
        (dc (lambda (dc x y)
              (send dc draw-rectangle x y (pict-width p) (pict-height p)))
            (pict-width p) (pict-height p) (pict-ascent p) (pict-descent p))
        color)
       p)
      p))
(define light-shade (make-object color% 195 195 255))
(define (darker c)
  (scale-color 0.95 c))

(define (itt s)
  (text s `(italic . ,(current-code-font)) (current-font-size)))

(define pattern (color-box (itt "pattern") light-shade))
(define template (color-box (itt "template") (darker light-shade)))

(define cloud-shade "lightgray") ; (make-object color% 195 255 195))
(define (encloud p shade)
  (let ([n (cc-superimpose
            (cloud (* 9/8 (pict-width p))
                   (* 3/2 (pict-height p))
                   shade)
            p)])
    (lift-above-baseline (drop-below-ascent n (- (pict-ascent p))) (- (pict-descent p)))))

(define-for-syntax (annotate-pat-tmpls loop light-shade cloud-shade pat--tmpls)
  (map (lambda (pt)
         (if (stx-pair? pt)
             (let ([tmpl (loop (stx-car (stx-cdr pt)) 
                               #`(darker (darker #,light-shade))
                               cloud-shade)]
                   [pat (stx-car pt)])
               (with-syntax ([us #'unsyntax])
                 (datum->syntax
                  pt
                  (list
                   (quasisyntax/loc pat
                     (us (color-box (code #,pat) #,light-shade)))
                   (quasisyntax/loc (stx-car (stx-cdr pt)) 
                     (us (color-box (code #,tmpl) (darker #,light-shade)))))
                  pt
                  pt)))
             (loop pt light-shade cloud-shade)))
       pat--tmpls))

;; The scode macro is like code, except that it
;;   - Shades behind patterns and templates (by recognizing keywords)
;;   - Puts clouds around expr for (code:encloud expr)
;; Much of the complexity has to do with preserving source-location
;;  info, which is crucial to proper typesetting of the code
;; >>> Some cut and paste here should be cleaned up! <<<
(define-syntax (scode stx)
  (syntax-case stx ()
    [(_ stx ...)
     #`(code
        #,@(map
            (lambda (stx)
              (let loop ([stx stx] [light-shade #'light-shade] [cloud-shade #'cloud-shade])
                (syntax-case stx (..... code:encloud)
                  [(ds n b)
                   (and (identifier? #'ds)
                        (free-identifier=? #'ds #'define-syntax))
                   (datum->syntax
                    stx
                    (list
                     #'ds
                     (loop #'n light-shade cloud-shade)
                     (loop #'b light-shade cloud-shade))
                    stx)]
                  [(dsr pat tmpl)
                   (and (identifier? #'dsr)
                        (or (free-identifier=? #'dsr #'define-syntax-rule)))
                   (datum->syntax
                    stx
                    (list*
                     #'dsr
                     (car
                      (annotate-pat-tmpls loop light-shade cloud-shade (list 
                                                                        (datum->syntax #f (list #'pat #'tmpl))))))
                    stx)]
                  [(sr pat--tmpl ...)
                   (and (identifier? #'sr)
                        (or (free-identifier=? #'sr #'identifier-syntax)))
                   (let ([pat--tmpls (syntax->list #'(pat--tmpl ...))])
                     (datum->syntax
                      stx
                      (list*
                       #'sr
                       (annotate-pat-tmpls loop light-shade cloud-shade pat--tmpls))
                      stx))]
                  [(sr kws pat--tmpl ...)
                   (and (identifier? #'sr)
                        (or (free-identifier=? #'sr #'syntax-rules)
                            (free-identifier=? #'sr #'syntax-id-rules)))
                   (let ([pat--tmpls (syntax->list #'(pat--tmpl ...))])
                     (datum->syntax
                      stx
                      (list*
                       #'sr
                       #'kws
                       (annotate-pat-tmpls loop light-shade cloud-shade pat--tmpls))
                      stx))]
                  [(sc expr kws pat--expr ...)
                   (and (identifier? #'sc)
                        (free-identifier=? #'sc #'syntax-case))
                   (let ([pat--exprs (syntax->list #'(pat--expr ...))])
                     (datum->syntax
                      stx
                      (list*
                       #'sc
                       (loop #'expr light-shade cloud-shade)
                       #'kws
                       (map (lambda (pe)
                              (if (stx-pair? pe)
                                  (let ([expr (stx-car (stx-cdr pe))]
                                        [pat (stx-car pe)])
                                    (with-syntax ([us #'unsyntax])
                                      (datum->syntax
                                       pe
                                       (list
                                        #`(us (datum->syntax
                                               #f
                                               (color-box (code #,pat) #,light-shade)
                                               (quote-syntax #,pat)))
                                        (loop expr light-shade cloud-shade))
                                       pe
                                       pe)))
                                  (loop pe light-shade cloud-shade)))
                            pat--exprs))
                      stx))]
                  [(sc expr pat--expr ...)
                   (and (identifier? #'sc)
                        (free-identifier=? #'sc #'syntax-parse))
                   (let ([pat--exprs (syntax->list #'(pat--expr ...))])
                     (datum->syntax
                      stx
                      (list*
                       #'sc
                       (loop #'expr light-shade cloud-shade)
                       (map (lambda (pe)
                              (if (stx-pair? pe)
                                  (let ([expr (stx-car (stx-cdr pe))]
                                        [pat (stx-car pe)])
                                    (with-syntax ([us #'unsyntax])
                                      (datum->syntax
                                       pe
                                       (list
                                        #`(us (datum->syntax
                                               #f
                                               (color-box (code #,pat) #,light-shade)
                                               (quote-syntax #,pat)))
                                        (loop expr light-shade cloud-shade))
                                       pe
                                       pe)))
                                  (loop pe light-shade cloud-shade)))
                            pat--exprs))
                      stx))]
                  [(ws pat--expr body)
                   (and (identifier? #'ws)
                        (free-identifier=? #'ws #'with-syntax))
                   (let ([pat--exprs (syntax->list #'pat--expr)])
                     (datum->syntax
                      stx
                      (list
                       #'ws
                       (datum->syntax
                        #'pat--expr
                        (map (lambda (pe)
                               (if (stx-pair? pe)
                                   (let ([expr (stx-car (stx-cdr pe))]
                                         [pat (stx-car pe)])
                                     (with-syntax ([us #'unsyntax])
                                       (datum->syntax
                                        pe
                                        (list
                                         #`(us (datum->syntax-object
                                                #f
                                                (color-box (code #,pat) #,light-shade)
                                                (quote-syntax #,pat)))
                                         (loop expr light-shade cloud-shade))
                                        pe)))
                                   (loop pe light-shade cloud-shade)))
                             pat--exprs)
                        #'pat--expr)
                       (loop #'body light-shade cloud-shade))
                      stx))]
                  [(sx tmplt)
                   (and (identifier? #'sx)
                        (free-identifier=? #'sx #'syntax))
                   (let ([tmpl (loop #'tmplt
                                     #`(darker (darker #,light-shade))
                                     cloud-shade)])
                     (datum->syntax
                      stx
                      (list
                       #'sx
                       (with-syntax ([us #'unsyntax])
                         #`(us (datum->syntax
                                #f
                                (color-box (code #,tmpl) (darker #,light-shade))
                                (quote-syntax tmplt)))))
                      stx))]
                  [(code:encloud x)
                   (with-syntax ([us #'unsyntax])
                     (quasisyntax/loc stx
                       (us (encloud (code #,(loop #'x light-shade
                                                  #`(darker #,cloud-shade)))
                                    #,cloud-shade))))]
                  [(a . b)
                   (datum->syntax
                    stx
                    (cons (loop #'a light-shade cloud-shade) (loop #'b light-shade cloud-shade))
                    stx
                    stx)]
                  [..... 
                   (with-syntax ([us #'unsyntax])
                     (quasisyntax/loc stx
                       (us .....-val)))]
                  [x #'x])))
            (syntax->list #'(stx ...))))]))

(define (introduced id)
  (colorize (parameterize ([code-colorize-enabled #f])
              (typeset-code id))
            RedColor))

(define .....-val (let ([p (code .....)])
                    (refocus (cc-superimpose 
                              (ghost p)
                              (scale (cloud (pict-width p) (pict-height p)) 0.95))
                             p))) 
