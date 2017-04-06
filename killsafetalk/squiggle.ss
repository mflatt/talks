(module squiggle (lib "slideshow.ss" "slideshow")
  (require (lib "math.ss")
	   (lib "class.ss")
	   (lib "mred.ss" "mred")
	   (lib "etc.ss")
	   "style.ss")

  (define squiggle 
    (opt-lambda (w h orig-points start-left? [init-swing (/ pi 4)])
      (define points
	(map (lambda (p)
	       (cons (- w (car p))
		     (- h (cdr p))))
	     orig-points))
      (define (one-squig points scale left? swing)
	(let* ([dx (- (caadr points) (caar points))]
	       [dy (- (cdadr points) (cdar points))]
	       [a ((if left? - +) (atan dy dx) swing)]
	       [r (* (sqrt (+ (* dx dx) (* dy dy))) scale)]
	       [cx (+ (caar points) (* (cos a) r))]
	       [cy (+ (cdar points) (* (sin a) r))])
	  (cons cx cy)))
      (define (squigs points swing iter)
	(let loop ([points points][left? start-left?][step 1][decay 1])
	  (list* (car points)
		 (one-squig points 1/2 left? (* decay swing))
		 (if (null? (cddr points))
		     (cdr points)
		     (if (= step iter) 
			 (loop (cdr points) (not left?) 1 (* decay 0.8))
			 (loop (cdr points) left? (add1 step) decay))))))
      (let ([squig-points (let loop ([points points][swing init-swing][step 1])
			    (if (< swing (/ pi 16))
				points
				(loop (squigs points swing step) (/ swing 2) (* 2 step))))])
	(picture
	 w h
	 (let loop ([points squig-points])
	   (let* ([ax (caar points)]
		  [ay (cdar points)]
		  [bx (caadr points)]
		  [by (cdadr points)])
	     (cons
	      `(connect ,ax ,(- h ay) ,bx ,(- h by) #f)
	      (if (null? (cddr points))
		  null
		  (loop (cdr points))))))))))

  (define t1-list '((10 . 98)
		    (10 . 80)
		    (10 . 50)
		    (10 . 40)
		    (25 . 30)
		    (40 . 8)
		    (41 . 8)
		    (44 . 20)
		    (45 . 30)))
  (define thread1 (squiggle 50 100 t1-list #t))
  (define short-rev-thread1 (squiggle 50 100 (map (lambda (p)
						    (cons (- 50 (car p)) 
							  (+ 20 (* 0.8 (cdr p)))))
						  t1-list) #f))
  (define shorter-thread1 (squiggle 50 100 (map (lambda (p)
						  (cons (- 50 (car p)) 
							(+ 45 (* 0.55 (cdr p)))))
						t1-list) #f))
  (define st1-list (map (lambda (p)
			  (cons (- 75 (* 1.5 (car p)))
				(cdr p)))
			t1-list))
  (define stretch-thread1 (squiggle 75 100 st1-list #f))

  (define t2-list '((35 . 95)
		    (35 . 80)
		    (35 . 40)
		    (40 . 20)
		    (30 . 10)
		    (25 . 25)
		    (22 . 30)
		    (15 . 25)
		    (10 . 10)
		    (8 . 20)
		    (7 . 22)
		    (3 . 50)))
  (define thread2 (squiggle 50 100 t2-list #t))
  (define short-thread2 (squiggle 50 100 (map (lambda (p)
						(cons (car p) (+ 20 (* 0.8 (cdr p)))))
					      t2-list)
				  #t))
  (define flipped-thread2 (squiggle 50 100 (map (lambda (p)
						  (cons (car p) (- 100 (cdr p))))
						t2-list) #f))
  (define hflipped-thread2 (squiggle 50 100 (map (lambda (p)
						   (cons (- 50 (car p)) (cdr p)))
						 t2-list) #f))
  (define squashed-hflipped-thread2 (squiggle 42 100 (map (lambda (p)
							    (cons (* 2/3 (- 50 (car p)))
								  (+ 30 (* 2/3 (cdr p)))))
							  t2-list) #f))

  (define t3-list '((10 . 85)
		    (20 . 95)
		    (30 . 90)
		    (30 . 70)
		    (30 . 50)
		    (30 . 35)
		    (20 . 45)
		    (40 . 50)))
  (define thread3 (squiggle 50 100 t3-list #t))
  (define short-thread3 (squiggle 50 100 (map (lambda (p)
					       (cons (car p)
						     (+ 20 (* 0.8 (cdr p)))))
					     t3-list) #t))
  (define stretch-thread3 (squiggle 75 100 (map (lambda (p)
						  (cons (+ (* 3/2 (car p)) 8)
							(- (cdr p) 20)))
						t3-list) #t))

  (define thread5-list '((10 . 25)
			 (50 . 30)
			 (100 . 20)
			 (140 . 40)))
  (define thread5 (squiggle 180 25
			    (map (lambda (p)
				   (cons (+ (car p) 10)
					 (- 25 (+ 7 (* (cdr p) 0.25)))))
				 thread5-list)
			    #f (/ pi 8)))
  (define mini-thread5 (squiggle 180 25
				 (map (lambda (p)
					(cons (+ (* 0.3 (car p)) 100)
					      (- 25 (+ 5 (* (cdr p) 0.25)))))
				      thread5-list)
				 #f (/ pi 8)))
  (define minimini-thread5 (squiggle 180 25
				     (map (lambda (p)
					    (cons (+ (* 0.15 (car p)) 100)
						  (- 25 (+ 5 (* (cdr p) 0.25)))))
					  thread5-list)
				     #f (/ pi 8)))

  (define thread4-list '((10 . 25)
			 (50 . 30)
			 (100 . 20)
			 (140 . 40)
			 (160 . 10)))
  (define thread4 (squiggle 180 50
			    thread4-list
			    #t))
  (define squashed-thread4 (squiggle 120 50
				     (map (lambda (p)
					    (cons (+ (* (car p) 2/3) 3)
						  (* (cdr p) 2/3)))
					  thread4-list)
				     #t))

  (define transparent-brush (send the-brush-list 
				  find-or-create-brush 
				  "white"
				  'transparent))
  

  (define (draw-arc dc x y w h sa fa)
    (send dc draw-arc x y w h sa fa))

  (define (draw-tri dc x y w h sa fa)
    (let ([pt (lambda (a r)
		(make-object point% 
			     (+ x (/ w 2) (+ (* (/ r 2) (cos a))))
			     (+ y (/ h 2) (- (* (/ r 2) (sin a))))))])
      (send dc draw-polygon (list
			     (pt sa w)
			     (pt (/ (+ sa fa) 2) (* 1.1 w))
			     (pt fa w))
	    0 0)))

  (define telecast
    (opt-lambda (w [orig-angle 0] [draw-arc draw-arc] [transparent? #t])
      (define angle (+ pi orig-angle))
      (let ([p (inset
		(dc (lambda (dc x y)
		      (let ([b (send dc get-brush)])
			(when transparent?
			  (send dc set-brush transparent-brush))
			(let ([limit w])
			  (let loop ([delta 0])
			    (let ([sweep (* 1/4 (/ (- limit delta) w))])
			      (unless ((+ delta 3) . > . limit)
				(draw-arc dc
					  (+ x (* (cos angle) (+ delta 10)))
					  (+ y (* (sin angle) (+ delta 10)))
					  10 10
					  (- (* pi (- 1 sweep)) angle) (- (* pi (+ 1 sweep)) angle))
				(loop (+ delta 3))))))
			(send dc set-brush b)))
		    w 10 0 0)
		10 0 0 0)])
	(if transparent?
	    (colorize (linewidth 1 p) "red")
	    (colorize p "blue")))))

  (define (fuzz p)
    (cc-superimpose
     (colorize (linewidth 3 p) thread-bg-color)
     (colorize (linewidth 1 p) thread-color)))

  (define (kline weak1? weak2?)
    (colorize
     (linewidth
      1
      (hline 180 1))
     (if (and weak1? weak2?) "light gray" "gray")))
  
  (define (mk-strong-pline h)
    (colorize (hc-append (linewidth 1 (vline 1 h))
			 (linewidth 1 (vline 1 h)))
	      "black"))
  (define strong-pline (mk-strong-pline 85))
  (define weak-pline
    (hc-append
     (blank 1 0)
     (colorize (linewidth 1 (vline 1 85)) "light gray")))

  (define flip-inset
    (case-lambda
     [(p l t r b) (inset p r b l t)]
     [(p h v) (inset p h v)]))
  (define flip-ct-superimpose cb-superimpose)
  (define flip-lc-superimpose rc-superimpose)
  (define flip-lb-superimpose rt-superimpose)
  (define (mk-flip-append alt)
    (lambda l
      (cond
       [(number? (car l)) (apply alt (car l) (reverse (cdr l)))]
       [else (apply alt (reverse l))])))
  (define flip-hb-append (mk-flip-append ht-append))
  (define flip-ht-append (mk-flip-append hb-append))
  (define flip-hc-append (mk-flip-append hc-append))
  (define flip-vc-append (mk-flip-append vc-append))
  (define flip-vl-append (mk-flip-append vr-append))
  (define flip-vr-append (mk-flip-append vl-append))

  (define (sys weak1? weak2? inner?)
    (frame
     (flip-ct-superimpose
      (flip-vc-append
       (blank 180 23)
       (if (and weak1? weak2?)
	   (blank 180 1)
	   (kline weak1? weak2?))
       (kline weak1? weak2?)       
       (flip-hb-append (blank 58 85)
		       (if weak1? weak-pline strong-pline)
		       (flip-vc-append
			(if inner?
			    (vc-append
			     (colorize (linewidth 1 (hline 120 1)) "gray")
			     (colorize (linewidth 1 (hline 120 1)) "gray"))
			    (blank 0 0))
			(flip-hb-append
			 (blank 58 60)
			 (if inner?
			     (mk-strong-pline 60)
			     (if weak2? weak-pline strong-pline))
			 (blank 60 60)))))
      (if (and weak1? weak2?)
	  (blank 0)
	  (flip-vl-append
	   (colorize (frame (blank 180 25)) "gray")
	   (flip-hb-append (blank 58 85) strong-pline))))))

  (define coop-sys
    (frame
     (flip-vc-append
      (blank 180 23)
      (kline #f #f)
      (kline #f #f)
      (flip-ht-append
       (blank 68 10)
       (flip-vc-append 
	(mk-strong-pline 5)
	(linewidth 2 (colorize (frame (blank 34 30)) "gray"))
	(mk-strong-pline 60))
       (blank 68 10)))))

  (define long-telecast
    (flip-ct-superimpose
     ; (flip-inset (telecast 80) 0 8 20 0)
     (flip-inset (telecast 20 (* 1/4 pi)) 80 10 0 0)
     (flip-inset (telecast 20 (- (* 1/4 pi))) 0 28 100 0)))

  (define unix
    (flip-ct-superimpose
     (cc-superimpose
      (sys #f #f #f)
      (flip-hc-append (flip-inset (fuzz short-thread2) 5 0) 
		      (flip-inset (fuzz thread3) 5 0) 
		      (flip-inset (fuzz short-rev-thread1) 5 0)))
     (fuzz thread5)
     long-telecast))

  (define jvm
    (flip-ct-superimpose
     (cc-superimpose
      (sys #t #t #f)
      (flip-inset (fuzz thread3) 40 0 0 0)
      (flip-inset (fuzz stretch-thread1) 80 0 0 0)
      (flip-inset (fuzz hflipped-thread2) 0 0 60 0))
     (flip-inset (telecast 15 0 draw-tri #f) 0 30 20 0)))
  
  (define mz
    (flip-ct-superimpose
     (flip-lc-superimpose
      (sys #f #t #t)
      (flip-hc-append
       (flip-inset (fuzz short-thread2) 4 0)
       (flip-lb-superimpose
	(flip-inset (fuzz short-thread3) 5 0 0 0)
	(flip-inset (fuzz shorter-thread1) 65 0 0 0))))
     (fuzz thread5)
     (flip-inset (fuzz mini-thread5) 0 25 0 0)
     long-telecast
     (flip-inset (telecast 15 (- (* 1/4 pi)) draw-tri #f) 8 48 0 0)
     (flip-inset (telecast 15 (* 1/2 pi) draw-tri #f) 104 32 0 0)))

  (define cmz
    (flip-ct-superimpose
     (cc-superimpose
      coop-sys
      (flip-hc-append
       (flip-inset (fuzz stretch-thread3) 0 20 70 0)
       (flip-inset (fuzz short-rev-thread1) -25 0 25 0)))
     (fuzz thread5)
     (flip-inset (fuzz minimini-thread5) 0 30 44 0)
     (flip-inset (telecast 15 0 draw-tri #f) 20 38 0 0)
     (flip-inset (telecast 15 0 draw-tri #f) 0 42 56 0)))

  (provide unix jvm mz cmz))

