(module angel-sequence (lib "slideshow.ss" "slideshow")
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "etc.ss")
           (lib "math.ss")
           (lib "step.ss" "slideshow")
           slideshow/balloon
           (only racket/runtime-path define-runtime-path))
  
  (provide use-angel-wings
           use-pl-labels
           angel-slides simpler-angel-slides simplest-angel-slides
           scheme-angel-file
           scheme-angel-meta-file
           devil-machine
           together-arrows
           dircomm
           short-dircomm
           mred-machine
           mred-logo)

  (define-runtime-path computer-png "computer.png")
  (define computer (bitmap computer-png))

  (define (desktop-machine* s f)
    (define p (desktop-machine s f))
    (scale computer (min (/ (pict-width p)
                            (pict-width computer))
                         (/ (pict-height p)
                            (pict-height computer)))))

  (define use-angel-wings (make-parameter #t))
  
  (define (halo w h)
    (dc (lambda (dc x y)
	  (let ([p (send dc get-pen)]
		[b (send dc get-brush)])
	    (send dc set-pen (find-pen "yellow" 4))
	    (send dc set-brush (find-brush "white" 'transparent))
	    (send dc draw-ellipse (+ x 2) (+ y 2) (- w 4) (- h 4))
	    (send dc set-brush b)
	    (send dc set-pen p)))
	w h 0 0))

  (define angel-file-icon (file-icon 80 115 #t #f))

  (define (mk-scheme-angel-file lambda?)
    (let ([fl (cc-superimpose
               angel-file-icon
               (if lambda? 
                   (scale (tt* "#lang " "..." "...") 0.5)
                   #;
                   (colorize (text "\u3bb" 'symbol 48) "blue")
                   (blank)))])
      (let ([wings
             (ht-append (angel-wing 55 80 #t)
                        (vc-append
                         10
                         (halo 75 25)
                         fl)
                        (angel-wing 55 80 #f))])
        (if (use-angel-wings)
            wings
            (pin-over (ghost wings)
                      fl lt-find
                      fl)))))

  (define scheme-angel-file/wings (delay (mk-scheme-angel-file #t)))
  (define scheme-angel-file/no-wings (delay (mk-scheme-angel-file #t)))

  (define (scheme-angel-file)
    (if (use-angel-wings)
        (force scheme-angel-file/wings)
        (force scheme-angel-file/no-wings)))

  (define (scheme-angel-meta-file)
    (let ([files (inset
		  (vl-append
		   (scheme-angel-file)
		   (inset (scheme-angel-file) font-size (/ font-size 2) 0 0))
		  (/ font-size 3))])
      (cb-superimpose
       (scale (mk-scheme-angel-file #f) 3.2 3)
       files)))
  
  #;
  (define mred-logo (bitmap (build-path (this-expression-source-directory) "plt-med-shield.gif")))
  (define mred-logo (scale (bitmap (collection-file-path "PLT-206.png" "icons")) 0.45))
  #;
  (define os-logo (bitmap (build-path (this-expression-source-directory) "windows.gif")))
  (define os-logo (scale (bitmap (build-path (this-expression-source-directory) "linux.png"))
                         0.5))

  (define machine-size 1.5)

  (define (devil-machine) (desktop-machine* machine-size (if (use-angel-wings)
                                                             '(devil)
                                                             '(binary))))
  (define mred-machine (desktop-machine* machine-size '(plt)))

  (define down-arrow (colorize (arrow 30 (- (/ pi 2))) "green"))
  (define downright-arrow (colorize (arrow 30 (- (/ pi 4))) "green"))
  (define downleft-arrow (colorize (arrow 30 (- (* pi 3/4))) "green"))

  (define ->pt (lambda (l)
		 (map (lambda (p)
			(make-object point% (car p) (cadr p)))
		      l)))

  (define (mk-bincomm s dir-w)
    (dc (lambda (dc x y)
	  (let-values ([(sx sy) (send dc get-scale)]
		       [(dx dy) (send dc get-origin)]
		       [(op) (send dc get-pen)]
		       [(ob) (send dc get-brush)])
	    (send dc set-origin (+ dx (* sx x)) (+ dy (* sy y)))
	    (send dc set-scale (* sx s) (* sy s))

	    (send dc set-brush
		  (send the-brush-list
			find-or-create-brush
			"yellow"
			'solid))
	    (send dc set-pen
		  (send the-pen-list
			find-or-create-pen
			"black"
			0
			'transparent))

	    (send dc draw-polygon
		  (->pt '((0 10)
			  (10 0)
			  (20 10)))
		  0 0)

	    (send dc draw-polygon
		  (->pt '((0 10)
			  (10 0)
			  (20 10)))
		  100 0)

	    (let ([r (make-object region% dc)]
		  [r2 (make-object region% dc)])
	      (send r set-ellipse 5 -50 110 110)
	      (send r2 set-ellipse 15 -40 90 90)
	      (send r subtract r2)
	      (send dc set-clipping-region r)
	      (send dc draw-rectangle 5 10 110 110)
	      (send dc set-clipping-region #f))
	    
	    (let ([s "1010101010"])
	      (send dc set-font (send the-font-list find-or-create-font
				      8 'modern 'normal 'normal))
	      (let-values ([(w h a d) (send dc get-text-extent s)])
		(send dc draw-text s (/ (- 120 w) 2) (- 65 h 4))))

	    (send dc set-pen op)
	    (send dc set-brush ob)

	    (send dc set-origin dx dy)
	    (send dc set-scale sx sy)))
	(* s 120) (* s 65) 0 0))

  (define (mk-dircomm s dir-w)
    (dc (lambda (dc x y)
	  (let-values ([(sx sy) (send dc get-scale)]
		       [(dx dy) (send dc get-origin)]
		       [(op) (send dc get-pen)]
		       [(ob) (send dc get-brush)])
	    (send dc set-origin (+ dx (* sx x)) (+ dy (* sy y)))
	    (send dc set-scale (* sx s) (* sy s))

	    (send dc set-brush
		  (send the-brush-list
			find-or-create-brush
			"yellow"
			'solid))
	    (send dc set-pen
		  (send the-pen-list
			find-or-create-pen
			"black"
			0
			'transparent))

	    (send dc draw-polygon
		  (->pt '((10 0)
			  (0 10)
			  (10 20)))
		  0 0)
	    (send dc draw-polygon
		  (->pt '((0 0)
			  (10 10)
			  (0 20)))
		  (- dir-w 5))

	    (send dc draw-rectangle
		  5 5 (- dir-w 10) 10)

	    (let ([s "(λ (x) ...)"]
		  [plain (send the-font-list find-or-create-font
			       6 'modern 'normal 'normal)])
	      (send dc set-font plain)
              (let*-values ([(w h d a) (send dc get-text-extent s)])
                (send dc draw-text s (/ (- dir-w w) 2) (/ (- 20 h) 2))))

	    (send dc set-pen op)
	    (send dc set-brush ob)
	    (send dc set-origin dx dy)
	    (send dc set-scale sx sy)))
	(* s (+ dir-w 5)) (* s 20) 0 0))

  (define bincomm (mk-bincomm 3 90))
  (define dircomm (mk-dircomm 3 90))
  (define short-dircomm (mk-dircomm 3 70))

  (define (center-wrt a p)
    (cc-superimpose p (blank (pict-width a) 1)))
    
  (define together-arrows
    (hc-append
     (pict-width dircomm)
     downright-arrow
     downleft-arrow))
    
  (define straight-arrows
    (hc-append
     font-size
     (center-wrt (scheme-angel-file) down-arrow)
     (ghost (launder dircomm))
     (center-wrt (scheme-angel-file) down-arrow)))

  (define (label-left s p sub)
    (let ([t (let ([p (text s `(bold . ,main-font) font-size)])
               (if sub
                   (refocus (vc-append (current-line-sep)
                                       p
                                       (t sub))
                            p)
                   p))]
          [sep (/ font-size 2)])
      (inset (hc-append sep p t) 0 0 (- 0 sep (pict-width t)) 0))) 
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (delta f dx dy)
    (lambda (p sp)
      (let-values ([(x y) (f p sp)])
        (values (+ x dx) (+ y dy)))))

  (define gnosys-tools
    (vl-append
     (current-line-sep)
     (t "contracts,")
     (t "static analysis,")
     (t "theorem proving")))

  (define (add-dircomm-analysis p)
    (pin-balloon (wrap-balloon
                  gnosys-tools
                  'ne (* 2 gap-size) (* -2 gap-size))
                 p
                 dircomm (delta lb-find 0 -10)))

  (define (angel-slides)
    (with-steps
        (angel #;analysis devil mred 
               angels angels-run bincomm 
               multi-mred dircomm #;dircomm-analysis 
               mredos)
      (slide/title
       (cond
         [(between? angel mred)
          (if (use-angel-wings)
              "Programming in Heaven"
              "Programming in a High-Level Language")]
         [(between? angels bincomm) "Multi-Programming"]
         [(between? multi-mred mredos) 
          (if (use-angel-wings)
              "Multi-Programming in Heaven"
              "Multi-Programming in a High-Level Language")])  
       ((if #f ; (only? dircomm-analysis)
            add-dircomm-analysis
            values)
        (cc-superimpose
         (vc-append
          (/ font-size 2)
          (hc-append
           font-size
           ((vafter angels) (scheme-angel-file))
           (cc-superimpose
            ((vbetween-excl angel angels) ((if #f ; (only? analysis)
                                               (lambda (p)
                                                 (pin-balloon (wrap-balloon
                                                               gnosys-tools
                                                               'ne gap-size (- gap-size))
                                                              p
                                                              angel-file-icon (delta lb-find -2 -10)))
                                               values)
                                           (scheme-angel-file)))
            ((vafter dircomm) dircomm))
           ((vafter angels) (scheme-angel-file)))
          (cc-superimpose
           ((vbetween-excl mred angels) down-arrow)
           ((vbetween-excl angels-run multi-mred) straight-arrows)
           ((vafter multi-mred) together-arrows))
          (hc-append
           font-size
           ((vbetween-excl angels-run multi-mred) (center-wrt (scheme-angel-file) mred-logo))
           (cc-superimpose
            ((vbetween mred mred) (center-wrt dircomm (label-left "language run-time" mred-logo
                                                                  "(Racket)")))
            ((vafter mredos) (label-left "language as OS" mred-machine #f))
            ((vbetween-excl multi-mred mredos) (center-wrt dircomm mred-logo)))
           ((vbetween-excl angels-run multi-mred) (center-wrt (scheme-angel-file) mred-logo)))
          (cc-superimpose
           ((vbetween mred mred) down-arrow)
           ((vbetween-excl multi-mred mredos) down-arrow)
           ((vbetween-excl angels-run multi-mred) together-arrows))
          ((vbetween-excl devil mredos) (devil-machine)))
         ((vbetween bincomm bincomm) bincomm))))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define use-pl-labels (make-parameter #f))

  (define (processes-title)
    (if (use-pl-labels)
        "Processes"
        "Machines and Processes"))

  (define (vm-label)
    (if (use-pl-labels)
        "programming language" 
        "virtual machine"))

  (define (simpler-angel-slides)
    (with-steps (machine binary devil programs os mred)
      (slide/title
       (processes-title)
       (vc-append
	font-size
	((vafter programs)
	 (hc-append
	  (pict-width dircomm)
	  (scheme-angel-file)
	  (scheme-angel-file)))
	((vafter os)
	 (vc-append
	  (/ font-size 2)
	  together-arrows
	  (hc-append
	   (cc-superimpose
	    ((vbefore mred) (center-wrt dircomm (label-left "operating system" os-logo #f)))
	    ((vafter mred) (center-wrt dircomm (label-left (vm-label) mred-logo #f)))))
	  down-arrow))
	(cc-superimpose
	 ((vonly machine) (desktop-machine* machine-size '()))
	 ((vonly binary) (desktop-machine* machine-size '(binary)))
	 ((vafter devil) (desktop-machine* machine-size '(devil))))))))

  (define (simplest-angel-slides)
    (with-steps (machine programs os mred)
      (slide/title
       (processes-title)
       (vc-append
	font-size
	((vafter programs)
	 (hc-append
	  (pict-width dircomm)
	  (scheme-angel-file)
	  (scheme-angel-file)))
	((vafter os)
	 (vc-append
	  (/ font-size 2)
	  together-arrows
	  (hc-append
	   (cc-superimpose
	    ((vbefore mred) (center-wrt dircomm (label-left "operating system" os-logo #f)))
	    ((vafter mred) (center-wrt dircomm (label-left (vm-label) mred-logo #f)))))
	  down-arrow))
        (desktop-machine* machine-size '()))))))
