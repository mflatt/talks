#lang slideshow
(require slideshow/code
         slideshow/balloon
         "contributor.rkt"
         "implementation.rkt"
         "utils.rkt")

(provide turnstile-slides)

(define (turnstile-slides #:show-rule? [show-rule? #t])
  (slide
   #:title "Extensible Type Systems"
   (para (bt "Typed Racket"))
   (item #:bullet (ghost bullet) "Expands, then check types")
   (item #:bullet (ghost bullet) "Type system is hardwired")
   'next
   (blank)
   (blank)
   'alts~
   (let ([mk
          (lambda (stephen? cite?)
            (list
             (let* ([ts  (bt "Turnstile")]
                    [p (para ts)])
               (if stephen?
                   (let ([p (pin-balloon (wrap-balloon (format-bear stephen-bear "Stephen" "Chang")
                                                       'sw (- gap-size) 0)
                                         p
                                         ts rc-find)])
                     (if cite? 
                         (add-cite p
                                   "Chang et al. [POPL'17]"
                                   #:page (blank client-w (* 1.1 client-h)))
                         p))
                   p))
             (item #:bullet (ghost bullet) (it "Extensible") "type checker")
             (item #:bullet (ghost bullet) "A term expands to its implementation plus type")))])
     (list (mk #f #f) (mk #t #t) (mk #t #f)))
   'alts~
   (let ([mk (lambda (lit? app?)
               (define (step arr? p)
                 (hbl-append gap-size ((if arr? values ghost) (t "→")) p))
               (list
                (code #,(step #f (code (+ 1 2)))
                      #,((if lit? values ghost) (step #t (code (+ (: 1 number) (: 2 number)))))
                      #,((if app? values ghost) (step #t (code (: (+ 1 2) number)))))))])
     (list (mk #f #f) (mk #t #f) (mk #t #t))))

  (when show-rule?
    (slide
     #:title "Turnstile"
     (code
      (define-m (checked-app e_fn e_arg)
        #:with (-> τ_in τ_out) (compute-τ e_fn)
        #:with τ_arg (compute-τ e_arg)
        #:when (τ= τ_arg τ_in)
        #:with ne_fn (erase-τ e_fn)
        #:with ne_arg (erase-τ e_arg)
        (add-τ (#%app ne_fn ne_arg) τ_out))))))
  
(module+ main
  (turnstile-slides))

  
