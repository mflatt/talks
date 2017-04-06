
(module spin-display mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "math.ss"))
  (provide rotate-a-little)
  
  (define degree 0)

  (define white-brush (send the-brush-list 
			    find-or-create-brush
			    "white"
			    'solid))
  
  (define orange-brush (send the-brush-list 
			     find-or-create-brush
			     "orange"
			     'solid))

  (define orange-pen (send the-pen-list 
			     find-or-create-pen
			     "orange"
			     2
			     'solid))
  
  (define-values (display-w display-h) (get-display-size))

  (define f (new frame% 
		 [label "Spinner"]
		 [x (- display-w 200)]
		 [y (- display-h 170)]
		 [style '(hide-menu-bar)]))
  (define c (instantiate canvas% (f)
			 [min-width 100]
			 [min-height 100]
			 [paint-callback
			  (lambda (c dc)
			    (send dc set-smoothing 'aligned)
			    (send dc set-brush white-brush)
			    (send dc set-pen orange-pen)
			    (send dc draw-ellipse 0 0 100 100)
			    (send dc set-brush orange-brush)
			    (send dc draw-arc 0 0 100 100 (- degree 0.1) (+ degree 0.1)))]))
			    
  (send f show #t)

  (define (rotate-a-little)
    (set! degree (- degree 0.05))
    (send c refresh)))

			 