#lang slideshow
(require slideshow/code
         "style.rkt")

(provide meaning-slides
         meaning-example-slides)

(define (stage orig-l)
  (let loop ([a null] [l orig-l])
    (cond
     [(null? (cdr l))
      (slide
       (let ([p (apply vl-append
                       (+ (* 2 (current-code-line-sep))
                          (pict-height (car orig-l)))
                       (cons (car l) a))])
         (scale p (min 1 
                       (/ client-w (pict-width p))
                       (/ (* 0.8 client-h) (pict-height p))))))]
     [else
      (loop a (list (car l)))
      (loop (cons (car l) a) (cdr l))])))
   

(define (meaning-slides)
  (stage
   (reverse
    (list
     (code
      (define-syntax define-syntax-rule
        (syntax-rules (...)
          [(define-syntax-rule
             (def-id (id a ...) expr)
             tmpl)
           (define-syntax def-id
             (syntax-rules ()
               [(def-id (id a ...) expr)
                (define-values (id) 
                  (lambda (a ...) "fish"))]))])))

     (code
      (define-syntax-rule
        (define (id a ...) expr)
        (define-values (id) 
          (lambda (a ...) (list a ...)))))

     (code
      (define (+ x y) (* x y)))

     (code 
      (+ 1 2))))))

(define (meaning-example-slides)
  (slide
   (code
    #,(tt "#lang") htdp/bsl
    code:blank
    1 #,(hilite (code +) #:color "pink") 2))

  (slide
   (code
    #,(tt "#lang") lazy
    code:blank
    (define ones (cons 1 ones))
    code:blank
    (list-ref ones 42)))

  (slide
   (scale
    (code
     #,(tt "#lang") plai-typed
     code:blank
     (define-syntax-rule (case expr [(d ...) rhs] ...)
       (let ([v expr])
         (cond
          [(or (equal? v 'd) ...) rhs]
          ...)))
     code:blank
     (case 'i
       [(i) 1]
       [(v) 5]
       [(x) 10]))
    0.8)))

(module+ main
  (meaning-example-slides))
