#lang slideshow
(require slideshow/play
         "person.rkt"
         "department.rkt"
         "res+edu.rkt"
         "util.rkt")

(provide res+edu-intro-slides)

(define (res+edu-intro-slides)
  (define prof1 (professor))
  (define the-students (make-students))

  (define g-prof1 (ghost prof1))
  (define g2-prof1 (ghost prof1))
  (define g-students (map ghost the-students))
  (define g2-students (map ghost the-students))

  (define department (make-department #:prof1 g-prof1
                                      #:students (group-students g-students)))

  (define dept-prof-s (extract-scale department g-prof1))
  (define dept-students-s (extract-scale department (car g-students)))

  (define classroom0 (classroom #:paper? #f
                                #:textbook-fade-n 1
                                #:professor g2-prof1
                                #:students g2-students))
  (define class-students-s (extract-scale classroom0 (car g2-students)))
  
  (play-n
   #:skip-first? #t
   #:skip-last? #t
   (lambda (n)
     (let* ([n (fast-middle n)]
            [prof-s (+ dept-prof-s (* n (- 1 dept-prof-s)))]
            [students-s (+ dept-students-s (* n (- class-students-s dept-students-s)))])
       (let* ([p (cc-superimpose
                  (over-cellophane (inset department gap-size) (- 1 n))
                  (classroom #:paper? #f
                             #:textbook-fade-n (- 1 n)
                             #:professor g2-prof1
                             #:students g2-students))]
              [p (slide-pict p (scale prof1 prof-s) g-prof1 g2-prof1 n)]
              [p (for/fold ([p p]) ([the-student (in-list the-students)]
                                    [g-student (in-list g-students)]
                                    [g2-student (in-list g2-students)])
                   (slide-pict p (scale the-student students-s) g-student g2-student n))])
         p)))))
                 
(module+ main
  (department-slides)
  (res+edu-intro-slides)
  (res+edu-slides))
