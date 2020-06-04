#lang slideshow
(require slideshow/play
         "color.rkt"
         "paper.rkt"
         "gear.rkt"
         "person.rkt")

(provide bar-slides
         department-view
         citizen-title
         reality-title)

(define BAR-W 150)
(define BAR-H 500)

(define (just-bar amt color)
  (linewidth
   #f
   (cellophane
    (colorize (filled-rectangle BAR-W (* BAR-H amt))
              color)
    medium-alpha)))

(define (bar amt
             #:color color
             #:extras [extras null]
             #:icon icon)
  (vc-append
   gap-size
   (inset
    (for/fold ([p (just-bar amt color)]) ([e (in-list (reverse extras))])
      (vc-append (just-bar (car e) (cadr e)) p))
    0 (* BAR-H (- 1 (+ amt (for/sum ([e (in-list extras)])
                             (car e)))))
    0 0)
   (ct-superimpose (blank BAR-W (* 3/4 BAR-W))
                   icon)))

(define (expectation #:res res-amt
                     #:edu edu-amt
                     #:prod prod-amt
                     #:prod-as-res [prod-as-res #f]
                     #:prod-as-edu [prod-as-edu #f]
                     #:prod-as-serv [prod-as-serv #f]
                     #:serv serv-amt
                     #:serv-visible [serv-visible 1])
  (hc-append
   BAR-W
   (bar res-amt
        #:color res-color
        #:icon (scale (paper) 0.7))
   (bar edu-amt
        #:color edu-color
        #:icon (scale (students) 0.4))
   (hc-append
    (* serv-visible BAR-W)
    (bar prod-amt
         #:color prod-color
         #:icon (scale (gear) 0.7)
         #:extras (append
                   (if prod-as-res (list (list prod-as-res res-color)) null)
                   (if prod-as-edu (list (list prod-as-edu edu-color)) null)
                   (if prod-as-serv (list (list prod-as-serv serv-color)) null)))
    (let ([serv (bar serv-amt
                     #:color serv-color
                     #:icon (scale (accountant) 0.8))])
      (if (= serv-visible 1)
          serv
          (let ([p (blank (* serv-visible (pict-width serv))
                          (pict-height serv))])
            (refocus (lt-superimpose p (cellophane serv serv-visible))
                     p)))))))

(define min-bar 0.01)

(define citizen-res 0.20)
(define citizen-edu 0.80)
(define citizen-prod min-bar)
(define citizen-serv min-bar)

(define dept-res 0.55)
(define dept-edu 0.30)
(define dept-prod min-bar)
(define dept-serv 0.15)

(define my-res 0.25)
(define my-edu 0.30)
(define my-prod 0.30)
(define my-serv 0.15)

(define my-prod-as-res 0.20)
(define my-prod-as-edu 0.05)
(define my-prod-as-serv 0.02)

(define (from-to n a b)
  (+ a (* n (- b a))))

(define the-prof (delay (professor)))
(define (add-prof-icon s #:prof [prof (force the-prof)])
  (hc-append gap-size (titlet s) (scale prof 0.6)))

(define (department-view-title)
  (add-prof-icon "Department View of Professors"))

(define (reality-title #:prof [prof (force the-prof)])
  (add-prof-icon "My Reality" #:prof prof))

(define department-view-at-n
  (lambda (n)
    (expectation #:res (from-to n citizen-res dept-res)
                 #:edu (from-to n citizen-edu dept-edu)
                 #:prod (from-to n citizen-prod dept-prod)
                 #:serv (from-to n citizen-serv dept-serv))))

(define (department-view)
  (vc-append
   gap-size
   (department-view-title)
   (department-view-at-n 1)))

(define (citizen-title [prof (force the-prof)])
  (add-prof-icon "Citizen View of Professors" #:prof prof))

(define (bar-slides)
  (play-n
   #:title (citizen-title)
   #:name "Citizen View of Professors"
   #:layout 'tall
   #:skip-last? #t
   (lambda (n)
     (expectation #:res (+ min-bar (* (- citizen-res min-bar) n))
                  #:edu citizen-edu
                  #:prod citizen-prod
                  #:serv citizen-serv
                  #:serv-visible 0)))
  
  (play-n
   #:title (add-prof-icon "Citizen View of Professors")
   #:name "Citizen View of Professors"
   #:layout 'tall
   (lambda (n)
     (expectation #:res citizen-res
                  #:edu citizen-edu
                  #:prod citizen-prod
                  #:serv citizen-serv
                  #:serv-visible n)))

  (play-n
   #:title (department-view-title)
   #:name "Department View of Professors"
   #:layout 'tall
   #:skip-first? #t
   department-view-at-n)

  (play-n
   #:title (reality-title)
   #:name "My Reality"
   #:layout 'tall
   #:skip-first? #t
   (lambda (n)
     (expectation #:res (from-to n dept-res my-res)
                  #:edu (from-to n dept-edu my-edu)
                  #:prod (from-to n dept-prod my-prod)
                  #:serv (from-to n dept-serv my-serv))))

  (play-n
   #:title (reality-title)
   #:name "My Reality"
   #:layout 'tall
   #:skip-first? #t
   (lambda (n)
     (expectation #:res (from-to n my-res (- my-res my-prod-as-res))
                  #:edu (from-to n my-edu (- my-edu my-prod-as-edu))
                  #:prod my-prod
                  #:prod-as-res (from-to n 0 my-prod-as-res)
                  #:prod-as-edu (from-to n 0 my-prod-as-edu)
                  #:prod-as-serv (from-to n 0 my-prod-as-serv)
                  #:serv (from-to n my-serv (- my-serv my-prod-as-serv))))))

(module+ main
  (bar-slides))
