(module mkns mzscheme
  (require (lib "etc.ss"))

  (provide v202-make-namespace-with-mred)

  (define v202-make-namespace-with-mred
    (opt-lambda ([flag 'mred])
      (unless (memq flag '(initial mred empty))
	(raise-type-error 'make-namespace-with-mred
			  "flag symbol, one of 'mred, 'initial, or 'empty"
			  flag))
      (let ([orig (current-namespace)]
	    [mred-name ((current-module-name-resolver)
			'(lib "mred.ss" "mred") #f #f)]
	    [ns (make-namespace (if (eq? flag 'empty) 'empty 'initial))])
	(parameterize ([current-namespace ns])
	  (namespace-attach-module orig mred-name)
	  (when (eq? flag 'mred)
	    (namespace-require '(lib "mred.ss" "mred"))
	    (namespace-require '(lib "class.ss"))))
	ns))))
