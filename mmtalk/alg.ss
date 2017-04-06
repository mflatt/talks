(module alg (lib "slideshow.ss" "texpict")

  (provide alg-code
	   alg-code*)
  
  (define alg-patterns
    (list
     ;; single-char tt:
     (regexp "^[#]")
     (lambda (s) (tt s))

     ;; single-char t:
     (regexp "^[ ']")
     (lambda (s) (t s))

     (regexp "^~")
     (lambda (s) (tt " "))

     (regexp "^[.][.][.]")
     (lambda (s) (tt "..."))

     (regexp "^[.][.]") ; condensed dots
     (lambda (s) (t "..."))

     (regexp "^[()]")
     (lambda (s) (t s))

     (regexp "^(define-syntax|define|let|require|require-for-syntax|for-syntax|module|define-record|record-switch)( |$)")
     (lambda (s) (bt s))

     (regexp "^[\"][^\"]*[\"]")
     (lambda (s) (t s))

     (regexp "^(@|!|)[a-zA-Z0-9.][-a-zA-Z0-9.?!]*")
     (lambda (s) (let* ([@? (char=? #\@ (string-ref s 0))]
			[!? (char=? #\! (string-ref s 0))]
			[p (t (if (or @? !?) (substring s 1 (string-length s)) s))])
		   (cond
		    [@? (colorize p "blue")]
		    [!? (colorize p "red")]
		    [else p])))))
		   

  (define (code/patterns patterns s)
    (let ([l (string-length s)])
      (let loop ([p 0])
	(if (= p l)
	    (blank)
	    (let ploop ([patterns patterns])
	      (cond
	       [(null? patterns) (error 'code "can't match: ~a" (substring s p l))]
	       [(regexp-match-positions (car patterns) s p)
		=> (lambda (m)
		     (hbl-append
		      ((cadr patterns) (substring s (caar m) (cdar m)))
		      (loop (cdar m))))]
	       [else (ploop (cddr patterns))]))))))

  (define (alg-code$ p s)
    (if (string=? s "")
	(t "")
	(let ([m (and p (regexp-match-positions p s))])
	  (if m
	      (hbl-append
	       (alg-code$ #f (substring s 0 (caar m)))
	       (colorize (alg-code$ #f (substring s (caar m) (cdar m)))
			 "purple")
	       (alg-code$ p (substring s (cdar m) (string-length s))))
	      (code/patterns alg-patterns s)))))

  (define (alg-code t)
    (alg-code$ #f t))

  (define (alg-code$* p . l)
    (apply
     vl-append
     line-sep
     (map (lambda (x) (if (string? x) (alg-code$ p x) x)) l)))

  (define (alg-code* . l)
    (apply alg-code$* #f l)))
