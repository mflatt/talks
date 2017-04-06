#lang slideshow
(require slideshow/code
         syntax/modread
         (prefix-in s: scribble/manual)
         "block.rkt"
         "in-file.rkt")

(provide replay-slides)

(define p (open-input-file "eval-log-save.rktl"))

(define (eval-box strs submod) 
  (define o (open-output-bytes))
  (define e (open-output-bytes))
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns]
                 [current-output-port o]
                 [current-error-port e])
    (with-handlers ([exn:fail? (lambda (x)
                                 (display (exn-message x) e))])
      (for ([pr (in-list strs)]
            [i (in-naturals)])
        (define p (open-input-string (cdr pr)))
        (port-count-lines! p)
        (parameterize ([error-print-source-location #f])
          (define e
            (with-module-reading-parameterization
             (lambda ()
               (parameterize ([read-accept-reader #t])
                 (define e (read-syntax t p))
                 (when (or (eof-object? e)
                           (not (eof-object? (read p))))
                   (error "bad module"))
                 (check-module-form e
                                    'module
                                    "module")))))
          (define path (path->complete-path (format "~a.rkt" (car pr))))
          (parameterize ([current-module-declare-name
                          (make-resolved-module-path path)])
            (eval e))
          (dynamic-require (if (and submod (positive? i))
                               `(submod ,path ,submod)
                               path)
                           0)))))
  (define (trunc p)
    (clip (inset p 0 0 (min 0 (- (* 0.8 client-w) (pict-width p))) 0)))
  (define (lines str)
    (filter (lambda (s) (not (equal? s "")))
            (regexp-split #rx"\n" str)))
  (let ([l (append
            (map (lambda (s) (trunc (colorize (tt s) "darkblue")))
                 (lines (get-output-string o)))
            (map (lambda (s) (trunc (colorize (it s) "red")))
                 (lines (get-output-string e))))])
    (if (null? l)
        (blank)
        (apply vl-append l))))

(define (combine p1 p2)
  (define p (vc-append gap-size p1 p2))
  (scale p (min 1 (/ client-h (pict-height p)))))

(define (replay-slides)
  (let loop ([prev-time #f] [prev-pict #f] [title "??"] [strs null] [submod #f])
    (cond
     [(regexp-match? #rx"^;; @" p)
      (define this-title
        (if (regexp-try-match #rx"^ > " p)
            (read-line p)
            title))
      (define this-submod
        (and (regexp-try-match #rx"^ ! " p)
             (begin0 (read p) (read-line p))))

      (define this-time (regexp-match #rx" [0-9:apm]+ ---" p))
      (read-line p)
      (define file (regexp-match #rx"/([^/]*)[.]rkt" (read-line p)))

      (define s (bytes->string/utf-8
                 (car (regexp-match #rx"[^;]*" p))))
      
      (define this-pict
        (let ([p (codeblock->pict
                  (s:codeblock s))])
          (mk-file #:name (cadr file)
                   #:suffix "rkt"
                   (scale p (min 1
                                 (/ (* client-w 3/4) (pict-width p))
                                 (/ (* client-h 1/3) (pict-height p)))))))
      
      (cond
       [(equal? this-time prev-time)
        (if (= (length strs) 2)
            ;; Hack: 3-module case is a cycle error:
            (loop this-time
                  prev-pict
                  this-title
                  (list (cons "any" "#lang racket (error \"cycle in loading modules\")"))
                  #f)
            (loop this-time
                  (if ((+ (pict-width this-pict) (pict-width prev-pict))
                       . < . 
                       (- client-w gap-size))
                      (ht-append gap-size this-pict prev-pict)
                      (let ([p (vc-append gap-size this-pict prev-pict)])
                        (scale p (min 1 (/ (* client-h 3/4) (pict-height p))))))
                  this-title
                  (cons (cons (cadr file) s) strs)
                  this-submod))]
       [else
        (when prev-pict
          (slide #:title title (combine prev-pict (eval-box strs submod))))
        (loop this-time this-pict this-title (list (cons (cadr file) s)) this-submod)])]
     [else
      (when prev-pict
        (slide #:title title (combine prev-pict (eval-box strs submod))))])))

(module+ main
  (replay-slides))

