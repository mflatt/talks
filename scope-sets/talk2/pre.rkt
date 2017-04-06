#lang slideshow

(provide pre)

(define (pre #:scale [s 1] #:indent [indent 0] . l)
  (let ([p
         (apply vl-append
                (current-line-sep)
                (let ([line->pict
                       (lambda (line)
                         (if (null? line)
                             (tt " ")
                             (let ([ln (apply htl-append (reverse line))])
                               (if (zero? indent)
                                   ln
                                   (htl-append (tt (make-string indent #\space)) ln)))))])
                  (let loop ([l l] [line null] [any? #f])
                    (cond
                      [(empty? l) 
                       (if (null? line)
                           null
                           (list (line->pict line)))]
                      [(equal? (car l) "\n")
                       (if any?
                           (cons (line->pict line)
                                 (loop (cdr l) null #t))
                           (loop (cdr l) line #f))]
                      [else (loop (cdr l) 
                                  (cons (if (pict? (car l))
                                            (car l)
                                            (tt (car l)))
                                        line)
                                  #t)]))))])
    (scale p s)))
