#lang slideshow
(require slideshow/play
         "person.rkt"
         "gear.rkt"
         "paper.rkt")

(provide department-slides
         make-department)

(define (stable #:professor [prof (professor)]
                students papers gear service)
  (vc-append
   gap-size
   prof
   (scale (hc-append (* 2 gap-size)
                     students
                     papers
                     gear
                     service)
          0.7)))

(define (make-department #:prof1 [prof1 (professor)]
                         #:prof2 [prof2 (professor)]
                         #:prof3 [prof3 (professor)]
                         #:students [five-students (students #:count 5)])
  (define a-gear (gear))
  (define a-service (accountant))
  (hc-append
   (* 6 gap-size)
   (stable #:professor prof1
           five-students
           (two-papers)
           (scale a-gear 0.7)
           (scale a-service 0.5))
   (vl-append
    (* 6 gap-size)
    (stable #:professor prof2
            (students #:count 3)
            (three-papers)
            (scale a-gear 0.5)
            (scale a-service 0.7))
    (inset
     (stable #:professor prof3
             (students #:count 2)
             (paper)
             (scale a-gear 2)
             (scale a-service 0.9))
     (* 2 gap-size) 0 0 0))))

(define (department-slides)
  (slide (make-department)))
                 
(module+ main
  (department-slides))
