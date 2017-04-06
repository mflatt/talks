(module bmhelp mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "etc.ss")
           (lib "math.ss"))

  (provide skewed)

  (define (skew-bitmap bm scale)
    (let* ([w (send bm get-width)]
           [h (send bm get-height)]
           [bm2 (make-object bitmap% w h)]
           [dc1 (make-object bitmap-dc% bm)]
           [dc2 (make-object bitmap-dc% bm2)]
           [c (make-object color%)])
      (send dc2 clear)
      (with-method ([get (dc1 get-pixel)]
		    [get2 (dc2 get-pixel)]
                    [put (dc2 set-pixel)]
                    [cr (c red)]
                    [cg (c green)]
                    [cb (c blue)]
                    [setc (c set)])
        (let loop ([i 0])
          (unless (= i w)
            (let* ([scale (- 1 (* (/ (- w i 1) w) (- 1 scale)))]
                   [ih (* h scale)]
                   [dj (/ (- h ih) 2)])
              (let loop ([j 0][r 255][g 255][b 255])
                (unless (= j h)
		  (let ([j2 (+ dj (* j scale))])
		    (let-values ([(r2 b2 g2)
				  (if (zero? i)
				      (values 255 255 255)
				      (begin
					(get2 (sub1 i) j2 c)
					(values (cr) (cg) (cb))))])
		      (get i j c)
		      (let ([r (quotient (+ r r2 (cr)) 3)]
			    [g (quotient (+ g g2 (cg)) 3)]
			    [b (quotient (+ b b2 (cb)) 3)])
			(setc r g b)
			(put i j2 c)
			(loop (add1 j) r g b)))))))
	    (loop (add1 i)))))
      (send dc1 set-bitmap #f)
      (send dc2 set-bitmap #f)
      bm2))

  (define threshold 30)

  ;; Original idea was to trim the white border, but that turns out to
  ;; be difficult, because white is not consistently white. So look for
  ;; sharp contrasts from the left to determine x,y alignment, then
  ;; add whitespace at the end.
  (define (trim bm)
    (let ([w (send bm get-width)]
	  [h (send bm get-height)]
	  [dc (make-object bitmap-dc% bm)]
	  [c (make-object color%)])
      (letrec ([find-non-blank
		(lambda (x y dx dy r g b)
		  (send dc get-pixel x y c)
		  (let ([r2 (send c red)]
			[g2 (send c green)]
			[b2 (send c blue)])
		    (if (or (< threshold (abs (- r r2)))
			    (< threshold (abs (- g g2)))
			    (< threshold (abs (- b b2))))
			0
			(+ 1 (find-non-blank (+ x dx) (+ y dy) dx dy r2 b2 g2)))))])
	(let* ([dl (- (find-non-blank 0 (quotient h 2) 1 0 255 255 255) 5)]
	       [dt 0]
	       [dr (- w 485 dl)]
	       [db (- h 395 dt)])
	  (let* ([bm2 (make-object bitmap% (- w dl dr) (- h dt db))])
	    (send dc set-bitmap bm2)
	    (send dc clear)
	    (send dc draw-bitmap-section bm 0 0 dl dt (- w dl dr) (- h dt db))
	    (send dc set-bitmap #f)
	    bm2)))))

  (define here (this-expression-source-directory))
  
  (define (prepare-skewed base-name)
    (let ([orig (build-path (format "~a.bmp" base-name))]
          [skewed (build-path (format ".pre-~a.xpm" base-name))])
      (unless (and (file-exists? skewed)
                   (>= (file-or-directory-modify-seconds skewed)
                       (file-or-directory-modify-seconds orig)))
	(let ([bm (make-object bitmap% orig)])
	  (unless (send bm ok?)
             (error 'bitmap "problem with bitmap: ~e" orig))
	  (send (trim bm) ; (skew-bitmap (trim bm) 0.8)
		save-file skewed 'xpm)))))
  
  (define (skewed base-name)
    ; (prepare-skewed base-name)
    (make-object bitmap% (build-path here (format "~a.bmp" base-name)))))
