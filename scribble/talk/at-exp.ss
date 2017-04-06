#lang slideshow
(require slideshow/code
         slideshow/play)

(provide at-exp-slides)

(define at-sign (colorize (tt "@") (current-base-color)))
(define op (code op))
(define op2 (code op_2))
(define (make-braces inside)
  (hbl-append (colorize (tt "{") (current-base-color))
              inside
              (colorize (tt "}") (current-base-color))))
(define braces (make-braces (colorize (tt "text mode") (current-literal-color))))
(define braces2 (make-braces (hbl-append (colorize (tt "in ") (current-literal-color))
                                         at-sign op braces
                                         (colorize (tt "!") (current-literal-color)))))
(define brackets (code [expr ...]))
(define expr (code expr))

(define (at-exp-slides)
  (let* ([tbl
          (table 3
                 (list
                  (hbl-append at-sign op braces) (t "=") (code (op "text mode"))
                  (hbl-append at-sign op brackets) (t "=") (code (op expr ...))
                  (hbl-append at-sign expr) (t "=") (code expr))
                 lt-superimpose lt-superimpose
                 (* 3 gap-size) (* 3 gap-size))]
         [two-line (lambda (a b)
                     (vl-append
                      (current-line-sep)
                      a
                      (rbl-superimpose
                       (blank (pict-width tbl) 0)
                       (hbl-append (* 3 gap-size) (t "=") b))))]
         [both
          (two-line
           (hbl-append at-sign op brackets braces) 
           (code (op expr ... "text mode")))]
         [inside
          (two-line
           (hbl-append at-sign op2 braces2)
           (code (#,op2 "in " (op "text mode") "!")))])
    (play-n
     #:title "@-Notation"
     (lambda (both-n inside-n)
       (vl-append
        (* 3 gap-size)
        tbl
        (cellophane both both-n)
        (cellophane inside inside-n))))))
