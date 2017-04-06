
(module ks (lib "run.ss" "slideshow")
  (require (lib "etc.ss")
	   (lib "face.ss" "texpict")
	   (lib "symbol.ss" "texpict")
	   (lib "balloon.ss" "texpict")
	   (lib "step.ss" "slideshow")
	   (lib "code.ss" "slideshow")
	   (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "math.ss")
	   (lib "list.ss")
	   "../killsafetalk/style.ss"
           "../mredtalk2/angel-sequence.ss")
  
  (define bg-color (make-object color% 210 250 180))
  
  (define (ks-bitmap file)
    (bitmap (build-path (this-expression-source-directory)
                        'up
                        "killsafetalk"
                        file)))
  
  (define (3/4page-item . l)
    (apply item (* 4/5 client-w) l)) 
  
  ;; ************************************************************
  ;; Kill safety
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Cake
  
  (define cupcake (ks-bitmap "cupcake.png"))
  (define knife (ks-bitmap "knife.jpg"))
  (define finger (ks-bitmap "pointing_finger.png"))
  
  (define split-cupcake
    (hc-append gap-size
	       (inset/clip cupcake 0 0 (- (/ (pict-width cupcake) 2)) 0)
	       (inset/clip cupcake (- (/ (pict-width cupcake) 2)) 0 0 0)))
  
  
  (define (sibling color direction cake? yummy?)
    (scale (face* (if cake? 'normal 'none)
		  (if yummy? 'tongue 'plain)
		  #f color 0 0 
		  (if cake? (direction 5) 0)
		  0) 
	   0.75))
  
  (define (cake-slides)
    (with-steps
        (hi cake! yummy brandish split point details)
      (slide/title/center
       "Children's Food-Sharing Protocol"
       (hc-append
        (let ([p (sibling "yellow" + (after? cake!) (after? yummy))])
          (lc-superimpose
           (hb-append
            (- gap-size)
            (ghost p)
            ((vafter point) finger))
           p))
        (cc-superimpose
         ((vafter split) split-cupcake)
         ((vbetween-excl cake! split) cupcake))
        (ht-append
         (- gap-size)
         ((vafter brandish) knife)
         (sibling "orange" - (after? cake!) (after? yummy))))
       (blank)
       (blank)
       (vl-append
        gap-size
        ((vafter details)
         (page-item* "By inspection, the protocol is fair"))
        ((vafter details)
         (page-item* "No parental supervision required"))))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Threads and queue
  
  (define (big-squiggle w h bg)
    (let ([one-squiggle
	   (dc (lambda (dc x y)
		 (let* ([inc (* 1/8 h)]
			[w/2 (* 1/2 w)]
			[xmid (+ x (/ w 2))])
		   (send dc draw-spline 
			 xmid y
			 (+ x w) (+ y inc)
			 xmid (+ y (* 2 inc)))
		   (send dc draw-spline 
			 xmid (+ y (* 2 inc))
			 x (+ y (* 3 inc))
			 xmid (+ y (* 4 inc)))
		   (send dc draw-spline 
			 xmid (+ y (* 4 inc))
			 (+ x w) (+ y (* 5 inc))
			 xmid (+ y (* 6 inc)))
		   (send dc draw-spline 
			 xmid (+ y (* 6 inc))
			 x (+ y (* 7 inc))
			 xmid (+ y (* 8 inc)))))
	       w h 0 0)])
      (cc-superimpose
       (bg (colorize (linewidth 7 one-squiggle) thread-bg-color))
       (linewidth 3 one-squiggle))))
  
  (define (down-arrow angle)
    (arrowhead gap-size (* pi angle)))
  
  (define full-squiggle (big-squiggle (* gap-size 4)
				      (* gap-size 18)
				      values))
  
  (define plain-full-squiggle (big-squiggle (* gap-size 4)
					    (* gap-size 18)
					    ghost))
  
  (define (mk-squiggle-1 full-squiggle)
    (cb-superimpose
     (inset (inset/clip full-squiggle 0 (* gap-size -3) 0 0) 0 0 0 (* 1/3 font-size))
     (down-arrow -1/3)))
  (define (mk-squiggle-2  full-squiggle)
    (lb-superimpose
     (inset (inset/clip full-squiggle 0 0 0 (* gap-size -3)) 0 0 0 (* 1/3 font-size))
     (inset (down-arrow -3/4) (* 1/5 font-size) 0 0 0)))
  (define (mk-squiggle-3  full-squiggle)
    (lb-superimpose
     (inset (inset/clip full-squiggle 0 (* gap-size -1) 0 (* gap-size -3)) 0 0 0 (* 1/3 font-size))
     (inset (down-arrow -3/4) (* 1/8 font-size) 0 0 0)))
  
  (define plain-squiggle-1 (mk-squiggle-1 plain-full-squiggle))
  (define plain-squiggle-2 (mk-squiggle-2 plain-full-squiggle))
  (define plain-squiggle-3 (mk-squiggle-3 plain-full-squiggle))
  
  (define squiggle-1 (colorize (mk-squiggle-1 full-squiggle) thread-color))
  (define squiggle-2 (colorize (mk-squiggle-2 full-squiggle) thread-color))
  (define squiggle-3 (colorize (mk-squiggle-3 full-squiggle) thread-color))
  
  (define ball (disk (* 2 gap-size)))
  
  (define (ball-queue . colors)
    (frame
     (inset
      (apply vc-append
	     (/ (pict-width ball) 2)
	     (map (lambda (color)
		    (if color
			(colorize ball color)
			(ghost ball)))
		  colors))
      (/ gap-size 4))))
  
  (define (add/get-ball top-color bottom-color l2r?)
    (vc-append (/ gap-size 2)
	       (if top-color
		   (colorize ball top-color)
		   (ghost ball))
	       (let ([box (blank (* gap-size 6) gap-size)])
		 (add-arrow-line 
		  (pict-height box)
		  box
		  box (if l2r? find-lc find-rc)
		  box (if l2r? find-rc find-lc)))
	       (if bottom-color
		   (if (eq? '... bottom-color)
		       (cc-superimpose (t "...") (ghost ball))
		       (colorize ball bottom-color))
		   (ghost ball))))
  
  
  (define sync-label (tt "synchronized"))
  
  (define (queue-process q color disable? no-thread?)
    (let* ([squiggle ((if no-thread? ghost values)
		      (colorize (scale plain-squiggle-3 0.8 0.8)
				(if disable?
				    disabled-thread-color
				    thread-color)))]
	   [q2 (hc-append (/ gap-size 2)
			  squiggle
			  q
			  (ghost squiggle))]
	   [bg ((if disable? ghost values)
		(colorize (filled-rounded-rectangle 
			   (+ (pict-width q2) gap-size)
			   (+ (pict-height q2) gap-size)
			   gap-size)
			  color))])
      (cc-superimpose bg q2)))
  
  (define small-scheme-angel-file (scale (scheme-angel-file) 0.5))

  (define (threads-slides)
    (with-steps
        (init add-blue with-blue get-green details 
              java stop-bad halfway stuck no-stop
              cml cml-halfway cml-ok kill-safe last-problem)
      (begin
        (slide/title/center
         (cond
           [(before? java) "Sharing among Processes"]
           [(before? cml) "Sharing in Java"]
           [else "Sharing in Concurrent ML"])
         (hc-append gap-size 
		    (ht-append
		     small-scheme-angel-file
		     squiggle-1)
                    (lb-superimpose
                     (hc-append 
                      gap-size 
                      ((vonly add-blue) (add/get-ball "blue" #f #t))
                      (hb-append
                       gap-size
                       (let* ([q (cc-superimpose
                                  ((vbefore with-blue) (ball-queue #f #f "red" "purple"))
                                  ((vonly with-blue) (ball-queue #f "blue" "red" "purple"))
                                  ((vbetween-excl get-green cml) (ball-queue #f "blue" "red" #f))
                                  ((vonly cml) (ball-queue #f "blue" "red" "purple"))
                                  ((vafter cml-halfway) (ball-queue #f "blue" "red" #f)))]
                              [q+label (place-over
                                        q
                                        (/ (- (pict-width q) (pict-width sync-label)) 2)
                                        (- 0 (pict-height sync-label) line-sep)
                                        ((vafter java) sync-label))]
                              [q+half (place-over
                                       (if (after? cml)
                                           q
                                           q+label)
                                       (- (pict-width q) (/ (pict-width ball) 2))
                                       (- (pict-height q) (pict-height ball) (/ gap-size 4))
                                       (let ([gb (colorize ball "purple")])
                                         (cc-superimpose
                                          ((vbetween halfway no-stop) gb)
                                          ((vonly cml-halfway) gb))))]
                              [q+thread (if (after? cml)
                                            (refocus 
                                             (queue-process q+half bg-color #f #f)
                                             q+half)
                                            q+half)])
			 (if (after? cml)
			     (refocus (ht-append
				       gap-size
				       q+thread
				       (scale small-scheme-angel-file
					      (/ (pict-width ball)
						 (pict-width small-scheme-angel-file))))
				      q+thread)
			     q+thread))
                       (cc-superimpose
                        ((vonly get-green) (add/get-ball #f "purple" #t))
                        ((vbetween halfway no-stop) (add/get-ball #f #f #t))
                        ((vonly cml-halfway) (add/get-ball #f #f #t)))))
                     (cc-superimpose
                      ((vbetween-excl stuck cml) (add/get-ball #f '... #f))
                      ((vafter cml-ok) (add/get-ball #f "purple" #f))))
		    (ht-append
		     (let ([stopped (vr-append
				     (inset/clip squiggle-2 0 0 0 (- (* 1/4 (pict-height squiggle-2))))
				     (linewidth 4 (colorize (hline gap-size 0) "red")))])
		       (ct-superimpose
			((vbefore halfway) squiggle-2)
			((vbetween-excl cml cml-halfway) squiggle-2)
			((vbetween halfway no-stop) stopped)
			((vafter cml-halfway) stopped)))
		     small-scheme-angel-file))
         (blank)
         (blank)
         (ct-superimpose
          ((vbetween-excl details java)
           (vl-append
            gap-size
            (page-item* "Queue should be safe and fair")
            (page-item* "Should require no kernel supervision")))
          ((vbefore cml)
           (vc-append
            gap-size
            ((vafter stop-bad)
             (with-font 
              `(italic . ,(current-main-font))
              (lambda ()
                (colorize
                 (page-para* (tt "Thread.stop") 
                             (blank) sym:implies (blank)
                             sync-label "isn't enough")
                 "red"))))
            (blank)
            ((vafter no-stop)
             (page-para* sym:therefore "Java has no" (tt "Thread.stop")))))
          ((vafter kill-safe)
           (vc-append
            gap-size
            (page-para* "Abstraction-as-process naturally supports termination")
            (blank)
            ((vafter last-problem)
             (colorize
              (page-para* "Remaining problem: who controls the abstraction's process?")
              "red")))))))))
  
  ;; Don't call `threads-slides' until `terminate-slides' and
  ;; `abstraction-slides' are defined
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Why Terminate
  
  (define (spider w h)
    (let ([a (scheme-angel-file)]) 
      (scale a
             (max (/ w (pict-width a))
                  (/ h (pict-height a))))))
  
  (define web-server (ks-bitmap "web-server.gif"))
  (define web-client (ks-bitmap "ie.png"))
  (define web-servlet (spider (pict-width web-client) (pict-height web-client)))
  
  (define make-web-server
    (opt-lambda (space-scale add-job [filter-line (lambda (x i) x)])
      (let ([ws web-server]
	    [ie web-servlet])
	(let ([ies (map add-job
			(map launder (list ie ie ie ie))
			'(1 2 3 4))])
	  (let ([p (vc-append
		    (* space-scale (* 2 (pict-height ws)))
		    ws
		    (apply
		     hc-append
		     (* 2 (pict-width ie))
		     ies))])
	    (car
	     (foldl (lambda (ie pn)
		      (cons
		       (cc-superimpose
			(car pn)
			(filter-line
			 (linewidth 2 (add-line (ghost (car pn))
						ws find-cb
						ie find-ct
						1 "black"))
			 (cdr pn)))
		       (add1 (cdr pn))))
		    (cons p 1) ies)))))))
  
  (define add-web-client
    (lambda (x i) (vc-append
		   x
		   (linewidth 2
			      (dash-vline 0 (pict-height x) (* 1/3 gap-size)))
		   web-client)))
  
  (define (cooperation-slides)
    (slide/title
     "Cooperating Children"
     (let ([l null])
       (let ([p (make-web-server 1/2 (lambda (x i)
				       (set! l (cons x l))
				       (add-web-client x i)))]) 
	 (let-values ([(lx ly) (find-rc p (caddr l))]
		      [(rx ry) (find-lc p (cadr l))])
	   (ct-superimpose
	    p
	    (inset (scale dircomm (/ (- rx lx) (pict-width dircomm)))
		   0 (- (pict-height p) ly))))))))

  ;; ************************************************************
  ;; Custodians
  
  (define each-little-thread (list plain-squiggle-1 plain-squiggle-3 plain-squiggle-2))
  
  (define (add-cust p color disable?)
    (cc-superimpose
     ((if disable? ghost values)
      (colorize
       (filled-rounded-rectangle 
	(+ gap-size (pict-width p))
	(+ gap-size (pict-height p))
	(/ gap-size 2))
       color))
     p))
  
  (define dim-bg-color (scale-color 0.85 bg-color))
  
  (define (add-tree-line t from to)
    (add-line t
	      from find-cb
	      to find-ct
	      1 "black"))
  
  (define tag1 (blank))
  (define tag2 (blank))
  (define tag3 (blank))
  
  (define subprocess-balls (ball-queue #f "blue" "red" "purple"))
  
  (define (mk-queue-subprocess color disable? no-thread?)
    (scale (queue-process subprocess-balls color disable? no-thread?)
	   0.35))
  
  (define (mk-little-threads tag)
    (scale (apply hc-append gap-size 
		  (if tag
		      (cons (cb-superimpose 
			     tag
			     (car each-little-thread))
			    (cdr each-little-thread))
		      each-little-thread))
	   1/5))
  
  (define (mk-process-raw x disable? threads? cust? sub? q? 
			  tag cust-color no-x?)
    (define little-threads (colorize (mk-little-threads tag)
				     (if disable?
					 disabled-thread-color
					 thread-color)))
    
    (let* ([x (if (or disable? no-x?) (ghost x) x)]
	   [p (if threads?
		  (vc-append (/ gap-size 2) x little-threads)
		  x)]
	   [p (if sub?
		  (let* ([sub (let ([lt (scale little-threads 0.5)])
				(add-cust lt dim-bg-color disable?))]
			 [sub1 (launder sub)]
			 [sub2 (launder sub)]
			 [sub3 (launder sub)]
			 [t (vc-append
			     (* 2 gap-size)
			     p
			     (hc-append
			      gap-size
			      sub1 sub2 sub3))])
		    (if disable?
			t
			(foldl (lambda (sub t)
				 (add-tree-line t p sub))
			       t
			       (list sub1 sub2 sub3))))
		  p)]
	   [p (if q?
		  (let* ([proc (mk-queue-subprocess dim-bg-color disable? #f)]
			 [p (vc-append (* 2 gap-size) p proc)])
		    (if (or tag disable?)
			p
			(foldl (lambda (little-thread p)
				 (add-tree-line p little-thread subprocess-balls))
			       p
			       each-little-thread)))
		  p)])
      (if cust?
	  (add-cust p cust-color disable?)
	  p)))
  
  (define (mk-process x disable? threads? cust? sub? q? tag)
    (refocus (mk-process-raw x disable? threads? cust? sub? q? tag bg-color #f) x))
  
  (define (unify p proc2 proc3 hide-left? hide-right?)
    (let-values ([(x2 y2) (find-rb p proc2)]
		 [(x3 y3) (find-lb p proc3)])
      (refocus
       (vc-append
	(ct-superimpose
	 (vc-append
	  (ghost p)
	  (inset
	   (let* ([h (+ (pict-height (mk-little-threads #f)) gap-size)])
	     (hc-append
	      (- x3 x2)
	      ((if hide-left? ghost values)
	       (colorize (filled-rectangle (pict-width proc2) h)
			 bg-color))
	      ((if hide-right? ghost values)
	       (colorize (filled-rectangle (pict-width proc3) h)
			 bg-color))))
	   0 0 0 (- (/ gap-size 2))))
	 p)
	(let ([h (- (pict-height proc2) gap-size)])
	  (ct-superimpose
	   (cb-superimpose
	    (let* ([p (colorize
		       (filled-rounded-rectangle 
			(+ (- x3 x2) (pict-width proc2) (pict-width proc3))
			h
			(/ gap-size 2))
		       bg-color)]
		   [p (if hide-left?
			  (inset (inset/clip p (- (add1 (/ (pict-width p) 2))) 0 0 0)
				 (add1 (/ (pict-width p) 2)) 0 0 0)
			  p)]
		   [p (if hide-right?
			  (inset (inset/clip p 0 0 (- (/ (pict-width p) 2)) 0)
				 0 0 (/ (pict-width p) 2) 0)
			  p)])
	      p)
	    ((if (and hide-left? hide-right?)
		 ghost
		 values)
	     (colorize
	      (filled-rounded-rectangle 
	       (- x3 x2)
	       (- h (/ gap-size 2))
	       (/ gap-size 2))
	      (if (or hide-left? hide-right?)
		  bg-color
		  dim-bg-color))))
	   (inset
	    (colorize
	     (filled-rounded-rectangle 
	      (- x3 x2)
	      (* 2 gap-size)
	      (/ gap-size 2))
	     "white")
	    0 (- (* 1.5 gap-size)) 0 0))))
       p)))
  
  (define (all-together p a b c)
    (let-values ([(xa ya) (find-lt p a)]
		 [(xb yb) (find-rb p b)]
		 [(xc yc) (find-rb p c)]
		 [(my-spider) (launder web-servlet)])
      (refocus
       (add-tree-line
	(ct-superimpose
	 (inset (colorize (filled-rounded-rectangle 
			   (+ (- xb xa) gap-size)
			   (+ (- ya yc) gap-size)
			   (/ gap-size 2))
			  bg-color)
		0 (- (- (pict-height p) ya) (/ gap-size 2)) 0 0)
	 (inset my-spider
		0 (+ (- (pict-height p) ya) (/ gap-size 2)) 0 0)
	 p)
	web-server my-spider)
       p)))
  
  (define generic-web-server (make-web-server 0.5 (lambda (x i) (mk-process x #f #t #t #f #f #f))))
  (define subproc-web-server (make-web-server 0.5 (lambda (x i) (mk-process x #f #t #t (= i 2) #f #f))))
  
  (define cust-explain
    (inset
     (hc-append
      gap-size
      (colorize (filled-rounded-rectangle (* 3 gap-size) (* 3 gap-size)) bg-color)
      (tt "=")
      (bt "custodian")
      (tt "=")
      (para* (* client-w 1/4) "capability to execute"))
     0 (* gap-size 6) 0 0))
  
  (define (manage-slides)
    (slide/title
     "Managing Processes and Threads"
     'alts 
     (list (list (make-web-server 0.5 add-web-client))
           (list (make-web-server 0.5 (lambda (x i) (mk-process x #f #t #f #f #f #f))))
           (list generic-web-server
                 cust-explain)
           (list (make-web-server 0.5 
                                  (lambda (x i) (mk-process x (= i 3) #t #t #f #f #f))
                                  (lambda (x i) (if (= i 3) (ghost x) x)))
                 cust-explain))))
  
  (define happy (scale (face 'happy "yellow") 1/5))
  (define unhappy (scale (face 'unhappy "orange") 1/5))
  
  (define conclusion
    (opt-lambda (strs good?)
      (page-para/r (vc-append
		    line-sep
		    (colorize (apply vc-append line-sep (map bt strs))
			      (if good? "forestgreen" "red"))
		    (if good? happy unhappy))
		   (blank gap-size 0))))
  
  (define (manage/cust-slides)
    (slide/title
     "Managing with Custodians"
     'alts 
     (list (list generic-web-server)
           (list subproc-web-server)
           (list (make-web-server 0.5 
                                  (lambda (x i) (mk-process x (= i 2) #t #t (= i 2) #f #f))
                                  (lambda (x i) (if (= i 2) (ghost x) x))))
           (list (make-web-server 0.5 (lambda (x i) (mk-process x #f #t #t #f (= i 2) #f))))
           (list (make-web-server 0.5 
                                  (lambda (x i) (mk-process x (= i 2) #t #t #f (= i 2) #f))
                                  (lambda (x i) (if (= i 2) (ghost x) x)))
                 (blank 0 (* gap-size 6))
                 (conclusion '("Queue terminated with servlet") #t)))))
  
  (define (non-solution-slide n what . content)
    (apply
     slide/name
     (if n
	 (format "Non-Solution #~a - ~a" n what)
	 (format "Solution - ~a" what))
     (let ([tit (lambda (s)
		  (text s `(bold . ,(current-main-font)) title-size))])
       (hbl-append (if n
		       (colorize
			(tit (format "Non-Solution #~a" n))
			"red")
		       (colorize
			(tit "Solution")
			"forestgreen"))
		   (titlet " \u2014 ")
		   (titlet what)))
     (blank)
     content))
  
  (define summary-bullet-1
    (3/4page-item "Concurrent ML primitives for thread communication"))
  (define summary-bullet-2
    (3/4page-item "Custodians for process hierarchy"))
  
  (define summary-bullet-2-1
    (3/4page-item "Manager thread for state"))
  
  (define (toward-solution-slides)
    (slide/title
     "Towards Kill Safety with Custodians"
     'alts 
     (list (list (make-web-server 0.5 (lambda (x i) (mk-process x #f #t #t #f (= i 2) #f))))
           (let* ([mk-p 
                   (lambda (hide-2?)
                     (let ([p (make-web-server 0.5 
                                               (lambda (x i) 
                                                 (let ([subp (mk-process-raw x (and hide-2? (= i 2)) #t #t #f (= i 2)
                                                                             (cond
                                                                               [(= i 2) tag1]
                                                                               [(= i 3) tag2]
                                                                               [else #f])
                                                                             bg-color
                                                                             #f)])
                                                   (let ([x (refocus
                                                             (if (= i 2)
                                                                 (rb-superimpose
                                                                  subp
                                                                  (inset tag3 0 0 0 (* 1/4 (pict-height subp))))
                                                                 subp)
                                                             x)])
                                                     x)))
                                               (lambda (x i) 
                                                 (if (and hide-2? (= i 2))
                                                     (ghost x)
                                                     x)))])
                       (let ([p (if hide-2?
                                    p
                                    (add-tree-line p tag1 subprocess-balls))])
                         (add-tree-line p tag2 subprocess-balls))))])
             (list 'alts
                   (list (list (mk-p #f))
                         (list (mk-p #t)
                               (blank 0 (* gap-size 6))
                               (conclusion '("Not kill-safe among servlets") #f))))))))
  
  ;; ----------------------------------------
  ;;  Solution & Non-Solutions
  
  (define (solution-slides)
    (let* ([proc2 #f]
           [proc3 #f]
           [mk-main-p (lambda (hide-left? hide-right? dim?)
                        (make-web-server 0.5 
                                         (lambda (x i) 
                                           (let ([p (mk-process-raw x 
                                                                    (or (and hide-left? (= i 2))
                                                                        (and hide-right? (= i 3)))
                                                                    #t #t #f #f
                                                                    (cond
                                                                      [(= i 2) tag1]
                                                                      [(= i 3) tag2]
                                                                      [else #f])
                                                                    (if (and dim? (<= 2 i 3))
                                                                        dim-bg-color
                                                                        bg-color)
								    #f #; (and dim? (<= 2 i 3)))])
                                             (cond
                                               [(= i 2) (set! proc2 p)]
                                               [(= i 3) (set! proc3 p)])
                                             (refocus p x)))
                                         (lambda (x i) 
                                           (if (or (and (or hide-left? dim?) (= i 2))
                                                   (and (or hide-right? dim?) (= i 3)))
                                               (ghost x)
                                               x))))]
           [queue-subprocess (mk-queue-subprocess bg-color #f #f)]
           [mk-p (lambda (main-p queue-subprocess left-lines? right-lines?)
                   (let* ([p (vc-append gap-size
                                        main-p 
                                        (ghost (mk-little-threads #f)) 
                                        queue-subprocess)]
                          [p (if left-lines?
                                 (add-tree-line p tag1 subprocess-balls)
                                 p)])
                     (if right-lines?
                         (add-tree-line p tag2 subprocess-balls)
                         p)))]
           [joint-custody (mk-p (unify (mk-main-p #f #f #f) proc2 proc3 #f #f)
                                (mk-queue-subprocess dim-bg-color #f #f)
                                #t #t)])
      
      
      ;; ----------------------------------------
      ;;  Non-solutions
      
      (with-steps
          (atomic bad)
        (non-solution-slide
         1 "Atomic Region"
         (mk-p (mk-main-p #f #f #f) (mk-queue-subprocess "yellow" #f #t) #t #t)
         (inset 
          (rt-superimpose
           (lt-superimpose
            (hc-append
             gap-size
             (colorize (filled-rounded-rectangle (* 3 gap-size) (* 3 gap-size)) "yellow")
             (tt "=")
             (bt "atomic"))
            (page-para " "))
           ((vafter bad)
            (conclusion '("Queue might harm" "other servlets") #f)))
          0 (- (* 3 gap-size)) 0 0)))
      
      (non-solution-slide
       2 "Disjoint Process"
       'alts
       (list
        (list (mk-p (mk-main-p #f #f #f) queue-subprocess #t #t))
        (list (mk-p (mk-main-p #t #f #f) queue-subprocess #f #t))
        (list (mk-p (mk-main-p #t #t #f) queue-subprocess #f #f)
              'next
              (inset (conclusion '("Queue runs forever") #f)
                     0 (- (* 3 gap-size)) 0 0))))
      
      (let ([queue-subprocess (mk-queue-subprocess dim-bg-color #f #f)])
        (non-solution-slide
         3 "Meta-Servlet"
         (all-together (mk-p (mk-main-p #f #f #t)
                             queue-subprocess #t #t)
                       proc2 proc3 queue-subprocess)
         'next
         (inset (conclusion '("Merely moves" "the \u201Ckernel\u201D") #f)
                0 (- (* 4 gap-size)) 0 0)))

      ;; ----------------------------------------
      ;;  Solution
      
      (non-solution-slide
       #f "Joint Custody"
       'alts
       (list
        (list joint-custody)
        (list (mk-p (unify (mk-main-p #t #f #f) proc2 proc3 #t #f)
                    (mk-queue-subprocess bg-color #f #f)
                    #f #t))
        (list joint-custody)
        (list (mk-p (unify (mk-main-p #f #t #f) proc2 proc3 #f #t)
                    (mk-queue-subprocess bg-color #f #f)
                    #t #f))
        (list (mk-p (unify (mk-main-p #t #t #f) proc2 proc3 #t #t)
                    (mk-queue-subprocess bg-color #t #f)
                    #f #f)
              (inset (conclusion '("Queue runs exactly"
                                   "as long as servlets") #t)
                     0 (- (* 3 gap-size)) 0 0))))
      
      (slide/title
       "Why a Thread can have Multiple Custodians"
       'alts
       (list 
        (list generic-web-server)
        (let ([left-only (lambda ()
                           (mk-p (unify (mk-main-p #f #f #f) proc2 proc3 #f #t)
                                 (mk-queue-subprocess bg-color #f #f)
                                 #t #f))]
              [left-gone (lambda ()
                           (mk-p (unify (mk-main-p #t #f #f) proc2 proc3 #t #t)
                                 (mk-queue-subprocess bg-color #t #f)
                                 #f #f))]
              [right-only (lambda ()
                            (mk-p (unify (mk-main-p #t #f #f) proc2 proc3 #t #f)
                                  (mk-queue-subprocess bg-color #f #f)
                                  #f #t))]
              [in-flight (lambda (dm p)
                           (let-values ([(lx ly) (find-rc p proc2)]
                                        [(rx ry) (find-lc p proc3)]
                                        [(a) (colorize (arrow gap-size 0) "green")])
                             (let ([p (place-over 
                                       p
                                       (+ (/ (+ lx rx (- (pict-width a))) 2)
                                          (* dm (pict-width a)))
                                       (- (pict-height p) ry)
                                       a)])
                               (add-tree-line p a subprocess-balls))))]
              [mostly-dead (inset
                            (colorize (page-para (blank gap-size 0) 
                                                 (bt "Queue is only") (bit "mostly dead"))
                                      "blue")
                            0 (/ gap-size 2) 0 0)])
          (list 'alts
                (list
                 (list (left-only))
                 (list (in-flight -1.5 (left-only)))
                 (list (in-flight 0 (left-only)))
                 (list (in-flight 0 (left-gone))
                       'next
                       mostly-dead)
                 (list (in-flight 1.5 (left-gone))
                       mostly-dead)
                 (list (right-only)
                       (inset
                        (colorize
                         (page-para/r (bt "Use queue") sym:implies
                                      (bt "grant custodian")
                                      (blank gap-size 0))
                         "blue")
                        0 (/ gap-size 2) 0 0)))))))
      
      
      (slide/title
       "Kill-Safe Abstractions"
       (page-para "A language to support abstractions:")
       summary-bullet-1
       summary-bullet-2
       (3/4page-item (colorize
                      (t "Operation to grant a thread another custodian")
                      "blue"))
       
       (blank)
       (colorize (hline (* client-w 0.85) 0) "blue")
       (blank)
       
       (page-para "Each abstraction:")
       summary-bullet-2-1
       (3/4page-item (colorize
                      (t "Each action grants custodian to manager thread")
                      "blue")))))
  
  ;; ************************************************************
  ;; Details
  
  (current-keyword-list (append (current-keyword-list)
				(list "parameterize" "if"
                                      
				      "spawn" 
				      "channel-send" "channel-recv"
				      "channel-send-evt" "channel-recv-evt"
				      "sync" "choice-evt" "wrap-evt" "nack-guard-evt" "guard-evt"
				      "thread-suspend" "thread-resume"
				      "make-custodian" "current-custodian" "custodian-shutdown-all")))
  
  
  #;
  (slide/title/center
   "Details (See Paper)"
   (page-item "Custodians granted through" (code thread-resume))
   (blank)
   (page-item "CML's" (code guard-evt) "a natural place for" (code thread-resume))
   (blank)
   (page-item "Improved" (code nack-guard-evt) "for two-step protocols")
   (blank)
   (page-item "Kill-safe does not always imply break-safe, nor vice-versa"))
  
  ;; ************************************************************
  ;; Complete example
  
  (define changed
    (case-lambda
      [(p color)
       (refocus
        (cc-superimpose (colorize (filled-rectangle (pict-width p) 
                                                    (pict-height p))
                                  color)
                        p)
        p)]
      [(p) (changed p "yellow")]))
  (define (unchanged p) (changed p "white"))
  
  (define (code-slides)
    (with-steps
        (old new)
      (slide/title
       (if (before? new)
           "A Thread-Safe Queue"
           "A Kill-Safe Queue")
       (lt-superimpose
        (scale/improve-new-text
         (code
          #,(if (before? new)
                (code (define-struct safe-q 
			(put-ch get-ch)))
                (code (define-struct safe-q 
			(#,(changed (code manager-t)) put-ch get-ch))))
          code:blank
          (define (safe-queue)
            (define q (queue))
            (define get-ch (channel))
            (define put-ch (channel))
            (define (q-loop)
              (sync
               (choice-evt
                (wrap-evt 
                 (channel-send get-ch (peek q))
                 (lambda () (get q)))
                (wrap-evt 
                 (channel-recv put-ch)
                 (lambda (v) (put q v)))))
              (q-loop))
            #,(if (before? new)
                  (code (spawn q-loop))
                  (changed
                   (code (define manager-t #,(unchanged (code (spawn q-loop)))))))
            #,(if (before? new)
                  (code (make-safe-q put-ch get-ch))
                  (code (make-safe-q #,(changed (code manager-t)) put-ch get-ch)))))
         0.75)
        (page-para/r
         (scale/improve-new-text
          (code
           (define (safe-get sq)
             #,(if (before? new)
                   (code
                    (channel-recv 
                     (safe-q-get-ch sq)))
                   (code
                    #,(changed (code (resume sq)))
                    (channel-recv 
                     (safe-q-get-ch sq)))))
           code:blank
           (define (safe-put sq v)
             #,(if (before? new)
                   (code
                    (channel-send 
                     (safe-q-put-ch sq) v))
                   (code
                    #,(changed (code (resume sq)))
                    (channel-send 
                     (safe-q-put-ch sq) v))))
           code:blank
           #,(if (before? new)
                 (blank)
                 (changed
                  (code (define (resume sq)
                          (thread-resume
                           (safe-q-manager-t sq)
                           (current-thread)))))))
          0.75))))))
  
  ;; ----------------------------------------
  
  (define (kill-safe-slides)
    (cooperation-slides)
    (cake-slides)
    (threads-slides)
    (manage-slides)
    (manage/cust-slides)
    (toward-solution-slides)
    (solution-slides)
    (code-slides))
  
  (provide kill-safe-slides))
