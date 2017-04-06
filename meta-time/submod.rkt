#lang slideshow
(require slideshow/code
         slideshow/balloon
         "style.rkt"
         "in-file.rkt"
         "config.rkt")

(provide test-submod-slides
         submod-prim-slides)

(define-syntax-rule (hcode c ...)
  (scale (code c ...) 0.85))

(define rackunit-code (code rackunit))

(define first-test-code
  (code (check-equal 0 (seconds->hours 0))))

(define hours-test-code
  (code
   (require #,rackunit-code)
   #,first-test-code
   (check-equal 1 (seconds->hours 3600))
   (check-equal 42 (seconds->hours 151200))))

(define module+-code (code module+))

(define (hours-submod-code name #:encloud? [encloud? #f])
  (let-syntax ([module+ (make-code-transformer #'module+-code)])
    (code
     (module+ #,name
       #,(if encloud?
             (encloud hours-test-code
                      #:color new-color)
             hours-test-code)))))

(define hours-code
  (lc-superimpose
   (code
    (provide current-hours)
    code:blank
    (define (current-hours)
      (seconds->hours (current-seconds)))
    code:blank
    (define (seconds->hours s)
      (quotient s (* 60 60))))
   (blank (pict-width (hours-submod-code (code test))) 0)))

(define (test-submod-slides)
  (slide
   #:title (if gpce-version?
               "Code that Needs Tests"
               "Scope and Tests")
   #:layout 'top
   (mk-file
    #:name "hours"
    #:suffix "rkt"
    (hcode
     #,hours-code)))

  (define (external-slide #:balloon? [balloon? #f])
    (define seconds->hours-code (code seconds->hours))
    (slide
     #:title "External Tests"
     #:layout 'top
     (vc-append
      (* 2 gap-size)
      (let* ([p (mk-file
                 #:name "hours"
                 #:suffix "rkt"
                 (hcode
                  #,hours-code
                  code:blank
                  (provide #,seconds->hours-code)))]
             [p (if balloon?
                    (pin-balloon
                     (wrap-balloon
                      (para #:width (* client-w 1/2)
                            #:fill? #f
                            "Forces export of internal function")
                      'sw 0 gap-size)
                     p
                     seconds->hours-code ct-find)
                    p)])
        p)
      (mk-file
       #:name "hours-test"
       #:suffix "rkt"
       (hcode
        #,hours-test-code)))))
  (external-slide)
  (external-slide #:balloon? #t)

  (define (internal-slide #:dep? [dep? #f]
                          #:run? [run? #f]
                          #:phase? [phase? #f])
    (slide
     #:title "Internal Tests Create Dependencies"
     #:layout 'top
     (mk-file
      #:name "hours"
      #:suffix "rkt"
      (let* ([p (hcode
                 #,hours-code
                 code:blank
                 (code:comment "tests:")
                 #,(if phase?
                       (encloud hours-test-code
                                #:color new-color)
                       hours-test-code))]
             [p (if dep?
                    (pin-balloon
                     (wrap-balloon
                      (para #:width (* client-w 1/2)
                            #:fill? #f
                            "Creates a library dependency on testing framework")
                      'sw 0 gap-size)
                     p
                     rackunit-code ct-find)
                    p)]
             [p (if run?
                    (pin-balloon
                     (wrap-balloon
                      (para #:width (* client-w 1/2)
                            #:fill? #f
                            "Test run every time library is used")
                      'sw 0 gap-size)
                     p
                     first-test-code ct-find)
                    p)])
        p))))
  (internal-slide)
  (internal-slide #:dep? #t)
  (internal-slide #:run? #t)
  (internal-slide #:phase? #t)

  (define (test-submod-slide name alts
                             #:encloud? [encloud? #f]
                             #:macro? [macro? #f])
    (slide
     #:title "Submodule Tests"
     #:layout 'top
     'alts
     (let ([mk
            (lambda (encloud?)
              (mk-file
               #:name "hours"
               #:suffix "rkt"
               (let* ([p (hcode
                          #,hours-code
                          code:blank
                          #,(hours-submod-code name #:encloud? encloud?))]
                      [p (if macro?
                             (pin-balloon
                              (wrap-balloon
                               (para #:width (* client-w 1/2)
                                     #:fill? #f
                                     (code module+) "is a macro that generates a submodule")
                               'sw 0 gap-size)
                              p
                              module+-code ct-find)
                             p)])
                 p)))])
       (if macro?
           (list (list (mk #f)))
           (list (list (mk encloud?)) (list (mk #f)))))
     (blank)
     'alts
     alts))

  (test-submod-slide
   #:encloud? #t
   (code test)
   (list
    (list
     (para (code #,(tt ">") (require (submod "hours.rkt" test)))))
    (list
     (para (tt "% raco test hours.rkt")))))

  (test-submod-slide
   (code main)
   (list
    (list
     (para (tt "% racket hours.rkt")))))

  (test-submod-slide
   #:macro? #t
   (code main)
   null))

;; ----------------------------------------

(define false-code (code #f))

(define (equiv-slide #:almost? [almost? #t]
                     #:submod? [submod? #f]
                     #:base? [base? #t]
                     #:multi? [multi? #f]
                     #:star? [star? #f]
                     #:balloon? [balloon? #f])
  (slide
   #:title "Module Primitives"
   (mk-file #:name "program"
            #:suffix "rkt"
            (code
             #,(tt "#lang") racket/base
             #,(if submod?
                   (if multi?
                       (code (module+ test
                               ....)
                             (module+ test
                               ....))
                       (code (module+ main
                               ....)))
                   (code ....))))
   (para (cc-superimpose ((if almost? ghost values) (bt "="))
                         ((if almost? values ghost) (bt "≈"))))
   (let ([p (mk-file #:name "program"
                     #:suffix "rkt"
                     (code
                      (module program racket/base
                        #,(if submod?
                              (if multi?
                                  (code (module test racket/base
                                          ....
                                          ....))
                                  (if base?
                                      (code (module main racket/base
                                              ....))
                                      (if star?
                                          (code (module* main #,false-code
                                                  ....))
                                          (code (module main #,false-code
                                                  ....)))))
                              (code ....)))))])
     (if balloon?
         (pin-balloon
          (wrap-balloon (para #:width (/ client-w 2)
                              #:fill? #f
                              false-code "means ``inherit enclosing bindings''")
                        'n 0 (/ gap-size -1))
          p
          false-code cb-find)
         p))))

(define (submod-prim-slides)
  (equiv-slide #:almost? #f)
  (equiv-slide #:submod? #t #:almost? #t)
  (equiv-slide #:submod? #t #:almost? #t #:multi? #t)
  (equiv-slide #:submod? #t)
  (equiv-slide #:submod? #t #:star? #t #:almost? #f #:base? #f)
  (equiv-slide #:submod? #t #:balloon? #t #:star? #t #:almost? #f #:base? #f))

(module+ main
  (test-submod-slides)
  #;(submod-prim-slides))
