(module demo-eval (lib "slideshow.ss" "slideshow")
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "etc.ss")
           "mkns.ss"
           (only racket/base log-error))
  
  (provide current-demo-directory
           demo-inset
           end-subtalk
           begin-subtalk
           subtalk-eval)
  
  (define current-demo-directory
    (make-parameter (this-expression-source-directory)))
  
  (set-use-background-frame! #t)
  (enable-click-advance! #f)
  (define demo-inset (make-slide-inset 0 0 0 200))

  (define subtalk-custodian #f)
  (define subtalk-eventspace #f)
  (define subtalk-namespace #f)

  (define (end-subtalk)
    (when subtalk-custodian
      (custodian-shutdown-all subtalk-custodian))
    (set! subtalk-custodian #f)
    (set! subtalk-eventspace #f)
    (set! subtalk-namespace #f))

  (define (begin-subtalk)
    (unless subtalk-custodian
      (set! subtalk-custodian (make-custodian))
      (parameterize ([current-custodian subtalk-custodian])
	(set! subtalk-eventspace (make-eventspace))
	(parameterize ([current-eventspace subtalk-eventspace])
	  (set! subtalk-namespace (v202-make-namespace-with-mred))
	  (queue-callback (lambda ()
                            (uncaught-exception-handler (lambda (exn)
                                                          ((error-display-handler)
                                                           (if (exn? exn)
                                                               (exn-message exn)
                                                               (format "uncaught: ~s" exn))
                                                           exn)
                                                          (abort-current-continuation
                                                           (default-continuation-prompt-tag)
                                                           void)))
                            (current-directory (current-demo-directory))
                            ;(current-load-relative-directory (current-demo-directory))
			    (current-namespace subtalk-namespace)
			    (print-struct #t)
                            (error-print-context-length 0)
			    (set-subtalk-ports)))))))

  (define (subtalk-eval expr)
    (parameterize ([current-eventspace subtalk-eventspace])
      (queue-callback (lambda () (eval expr)))))

  (define plain-style (make-object style-delta% 'change-normal))
  (define red-it-style (send (make-object style-delta% 'change-style 'italic)
			     set-delta-foreground
			     "red"))

  (send plain-style set-delta 'change-size 24)
  (send red-it-style set-delta 'change-size 24)

  (define (set-subtalk-ports)
    (let ([ow #f]
	  [e #f])
      (let ([mk-write (lambda (style)
			(lambda (s start end nonblock? w/break?)
			  (let-values ([(s) (subbytes s start end)]
				       [(display-w display-h) (get-display-size)])
			    (unless ow
			      (set! ow (make-object frame% "stdout" #f
						    575 200 10 (- display-h 200)
						    '(no-caption no-resize-border hide-menu-bar)))
			      (set! e (make-object text%))
			      (make-object editor-canvas% ow e '(hide-hscroll hide-vscroll))
			      (send ow show #t))
			    (let ([orig (send e last-position)])
			      (send e begin-edit-sequence)
			      (send e insert (bytes->string/utf-8 s) orig)
			      (send e change-style style orig (send e last-position))
			      (send e end-edit-sequence)))
			  (- end start)))])
	(current-output-port (make-output-port
			      'subtalk
			      always-evt
			      (mk-write plain-style)
			      void))
	(current-error-port (make-output-port
			     'subtalk
			     always-evt
			     (mk-write red-it-style)
			     void))))))
