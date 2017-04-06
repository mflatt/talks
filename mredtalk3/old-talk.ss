
(module talk (lib "slideshow.ss" "slideshow")
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "math.ss")
	   (lib "list.ss")
	   (lib "etc.ss")
           (lib "step.ss" "slideshow"))

  (define os-title? #f)
  (define chinese-name? #f)
  (define edited-for-content? #f)
  (define short-angel-sequence? #f)
  (define non-drscheme-examples? #t)
  (define kill-safe-section? #f)
  (define long-conclusion? #f)

  (bitmap-draft-mode #f)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (author who where)
    (vc-append
     line-sep
     (colorize (bt who) "blue")
     (blank (/ font-size 3))
     (scale/improve-new-text
      (t where)
      0.8)))

  (slide/center
   (vc-append
    line-sep
    (titlet (if os-title?
                "Programming Languages as Operating Systems"
                "Processes without Partitions")))
   (blank)
   (bitmap (build-path (collection-path "icons") "PLT-206.png"))
   (blank)
   (author  (string-append "Matthew Flatt" 
                           (if chinese-name? "   马晓" ""))
            "University of Utah")
   (blank)
   (blank)
   (scale/improve-new-text
    (let ([a (author "Adam Wick" "University of Utah")]
          [r (author "Robert Bruce Findler" "Northwestern University")])
      (hc-append
       (* 3 gap-size)
       (cc-superimpose (ghost r) a)
       (cc-superimpose (ghost a) r)))
    0.80))
    
  (require "../mredtalk2/modified.ss")
  (when edited-for-content?
    (modified-slide))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (require "../mredtalk2/demo-eval.ss")
  (current-demo-directory (build-path (this-expression-source-directory)
                                      'up
                                      "mredtalk2"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define implies (text "\336" 'symbol font-size))

  (define tt-size
    (case font-size
      [(24) 20]
      [else 24]))
  
  (define (xtt s)
    (text s `(bold . modern) tt-size))
  (define (xtt* . x)
    (apply vl-append line-sep (map xtt x)))

  (define (half-page-item . s)
    (apply item (* client-w 3/4) s))

  (define brite-color "purple")
  (define dim-color "dark gray")
  (define hilite-color "black")

  (define (make-link s)
    (let ([p (t s)])
      (lift
       (colorize
	(vc-append
	 p
	 (linewidth 2 (hline (pict-width p) 1)))
	"blue")
       (- (+ 2 (pict-descent p))))))

  (define eval-link (make-link "eval"))
  
  (define (demo-slide/title s thunk . x)
    (slide/title/inset
     s
     demo-inset
     (lbl-superimpose
      (ct-superimpose
       (apply-slide-inset demo-inset titleless-page)
       (apply
	vl-append
	font-size
	x))
      (clickback eval-link thunk))))

  (define (mk-code-eval code)
    (lambda ()
      (begin-subtalk)
      (subtalk-eval (read (open-input-string 
			   (format "(begin ~a)"
				   (apply string-append code)))))))

  (define eval-code-slide
    (lambda (title . code)
      (demo-slide/title
       title
       (mk-code-eval (filter string? code))
       (apply tt*/normal code))))

  (define (eval-code-block/sep sep . code)
    (hbl-append
     sep
     (apply tt*/normal code)
     (clickback
      eval-link
      (mk-code-eval (filter string? code)))))

  (define (eval-code-block . code)
    (apply eval-code-block/sep font-size code))

  (define (tt*/dimmer dimmer l)
    (apply
     vl-append
     line-sep
     (let loop ([l l])
       (cond
	[(null? l) null]
	[(or (null? (cdr l)) (string? (cadr l)))
	 (cons (dimmer (xtt (car l))) (loop (cdr l)))]
	[else
	 (cons (colorize (let loop ([s (car l)])
			   (cond 
			    [(number? (cadr l))
			     (let ([len (string-length s)]
				   [out (if (negative? (cadr l))
					    dimmer
					    values)]
				   [in (if (negative? (cadr l))
					   values
					   dimmer)]
				   [n (abs (cadr l))])
			       (hbl-append 
				(in (xtt (substring s 0 (- len n))))
				(out (xtt (substring s (- len n) (- len (sub1 n)))))
				(in (xtt (substring s (- len (sub1 n)) len)))))]
			    [(eq? (cadr l) '<<) (xtt s)]
			    [(and (pair? (cadr l)) (string? (caadr l)))
			     (let ([m (regexp-match-positions (caadr l) s)])
			       (if m
				   (hbl-append
				    (xtt (substring s 0 (caar m)))
				    (colorize (xtt (substring s (caar m) (cdar m))) brite-color)
				    (loop (substring s (cdar m) (string-length s))))
				   (xtt s)))]
			    [else (error "bad dimmer")]))
			 hilite-color)
	       (loop (cddr l)))]))))

  (define (tt*/dim . l)
    (tt*/dimmer (lambda (x) (colorize x dim-color)) l))

  (define (tt*/normal . l)
    (tt*/dimmer values l))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (require "../mredtalk2/angel-sequence.ss")
  (if short-angel-sequence?
      (simpler-angel-slides)
      (angel-slides))

  (unless non-drscheme-examples?
    (slide/title/center
     "Languages as Operating Systems"
     (vc-append
      font-size
      (page-para "Language as OS"  implies "process controls:")
      (blank)
      (vl-append
       font-size
       (page-item* "Separate threads of evaluation")
       (page-item* "Separate process-specific state (e.g., current directory)")
       (page-item* "Separate graphical event loops")
       (page-item* "Separate debugging capabilities")
       (page-item* "Ability to terminate a process and reclaim its resources")))))
    

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (mrt2-bitmap file)
    (bitmap (build-path (this-expression-source-directory)
                        'up
                        "mredtalk2"
                        file)))
                       
  (define (file-label p s)
    (vc-append
     line-sep
     p
     (bt s)))
  
  (define prog-process-title "Process Examples")

  (when non-drscheme-examples?
    (slide/title/center
     prog-process-title 
     (rb-superimpose
      (inset (bitmap "firefox.png") 0 0 (* gap-size 3) (* gap-size 4))
      (bitmap "terminal.png"))))

  #;
  (slide/title/center
   prog-process-title 
   (mrt2-bitmap "installer.bmp"))
  
  (when non-drscheme-examples?
    (slide/title/center
     prog-process-title   
     (bitmap "web-break.png")))
  
  (when non-drscheme-examples?
    (slide/title/center
     prog-process-title
     (let ([ws (mrt2-bitmap "web-server.gif")]
           [ie (mrt2-bitmap "ie.bmp")])
       (let ([ies (map launder (list ie ie ie ie))])
         (let ([p (vc-append
                   (* 2 (pict-height ws))
                   ws
                   (apply
                    hc-append
                    (* 2 (pict-width ie))
                    ies))])
           (foldl (lambda (ie p)
		    (add-line p
			      ws find-cb
			      ie find-ct
			      1 dim-color))
                  p ies))))))

  (when non-drscheme-examples?
    (slide/title/center
     prog-process-title
     (mrt2-bitmap "sirmail.bmp")))
  
  (define the-scheme-angel-file (scheme-angel-file))
  
  (define (mk-drs-layout s)
    (let* ([user-file (ghost the-scheme-angel-file)]
	   [main
	    (vc-append
	     (/ font-size 2)
	     (hc-append
	      (file-label the-scheme-angel-file "DrScheme")
	      (ghost dircomm)
	      (file-label user-file "user's program"))
	     (blank)
	     together-arrows
	     mred-logo)])
      (let-values ([(x y) (find-lb main user-file)])
	(cons-picture
	 main
	 `((place ,x ,y
		  ,(let* ([u the-scheme-angel-file]
			  [big (scale u s s)])
		     (inset big 
			    (/ (- (pict-width u) (pict-width big)) 2)
			    (/ (- (pict-height u) (pict-height big)) 2)))))))))

  (slide/title
   prog-process-title
   (blank)
   (blank)
   (mk-drs-layout 1)
   (blank)
   (blank)
   (blank)
   (page-para
    (clickback
     (make-link "Run DrScheme")
     (lambda ()
       (end-subtalk)
       (begin-subtalk)
       (subtalk-eval
        `(parameterize ([current-command-line-arguments (vector "demo.scm")])
           (exit-handler (lambda (x) (,end-subtalk)))
           (dynamic-require '(lib "drscheme.ss" "drscheme") #f)))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

  (define object-color "forest green")
  (define (mk-object size)
    (cloud (* size 1.5 font-size) (* size font-size) object-color))
  
  (define (mk-partition-icon objs)
    (frame (inset (hc-append 
		   font-size
		   the-scheme-angel-file
		   objs)
		  (/ font-size 2))))
  
  (define (connect-all p o . tos)
    (let loop ([p p][tos tos])
      (if (null? tos)
	  p
	  (loop
	   (add-line p o find-cb
		     (car tos) find-ct
		     1
		     object-color)
	   (cdr tos)))))

  (define memory-icon
    (let* ([a1 (launder the-scheme-angel-file)]
	   [a2 (launder the-scheme-angel-file)]
	   [o1 (mk-object 1)]
	   [o2 (mk-object 1.2)]
	   [o3 (mk-object 1.3)]
	   [o4 (mk-object 1)]
	   [o5 (mk-object 1.4)]
	   [o6 (mk-object 1.25)]
	   [p (vc-append
	       font-size
	       (hc-append (* 0.5 (pict-width a1)) a1 a2)
	       (blank)
	       (hc-append font-size (ghost (launder o3)) o1 (blank) o2)
	       (blank)
	       (hc-append font-size o5 (ghost (launder o1)) o3 (ghost (launder o2)) o4 o6))])
      (connect-all (connect-all (connect-all (connect-all p a1 o1 o5) a2 o1 o2) o2 o6 o4) o1 o3)))

  (define unix-memory-icon
    (hc-append
     font-size
     (mk-partition-icon (vc-append
			 (mk-object 2)
			 (hc-append (mk-object 1) (mk-object 1.2))))
     (mk-partition-icon (vc-append
			 (hc-append
			  (vc-append (mk-object 1) (mk-object 1.2))
			  (mk-object 2))
			 (mk-object 1.3)))))

  (define (cite what who)
    (if who
	(hbl-append (bt what) (t "  ") (colorize (t (format "[~a]" who)) dim-color))
	(bt what)))

  (define term-title "Languages with Termination")

  (slide/title
   term-title
   (table 2
	  (list
	   (cite "Pilot" "Redell80")
	   (cite "SPIN" "Bershad95")
	   (cite "JKernel" "Hawblitzel98")
	   (cite "Alta" "Tullman99")
	   (cite "KaffeOS" "Back00")
	   (cite "JSR-121" "Soper03")
	   (cite ".NET application domains" #f)
	   (cite "..." #f))
	  lbl-superimpose lbl-superimpose
	  (* 4 gap-size) (* 2 gap-size))
   'next
   (blank)
   (blank)
   unix-memory-icon)

  (slide/title
   term-title
   (cite "PLT Scheme" #f)
   (blank)
   (blank)
   memory-icon)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define outline
    (apply
     make-outline 
     'motivation "Motivation and Approach" #f
     'plt-scheme "Processes in PLT Scheme" 
     (lambda (sym)
       (vl-append
        (* 3 line-sep)
        (half-page-item "Threads")
        (half-page-item "Parameters")
        (half-page-item "Eventspaces")
        (half-page-item "Custodians")))
     'accounting "Memory Accounting"
     (lambda (sym)
       (half-page-item "Without partitions" 
                       (blank font-size)
                       (colorize (t "[ISMM 04]") dim-color)))
     (if kill-safe-section?
         (list
          'kill-safe "Synchronization Abstractions"
          (lambda (sym)
            (half-page-item "From thread-safe to kill-safe"
                            (blank font-size)
                            (colorize (t "[PLDI 04]") dim-color))))
         null)))

  (outline 'plt-scheme)

  (define (aspect s)
    (colorize
     (page-para* 
      ;; capitalize s:
      (it (string-append (string (char-upcase (string-ref s 0))) (substring s 1 (string-length s)))))
     "blue"))


  (slide/title/tall/inset
   "Threads"
   demo-inset
   (aspect "concurrent execution")
   (blank)
   (vl-append
    (hbl-append
     font-size
     (xtt* 
      "(require \"spin-display.scm\")")
     (clickback
      eval-link
      (lambda ()
	(end-subtalk)
	(begin-subtalk)
	(subtalk-eval '(require "spin-display.scm")))))
    (eval-code-block
     " "
     "(define (spin)"
     "  (rotate-a-little)"
     "  (sleep 0.1)"
     "  (spin))"
     " "
     "(define spinner (thread spin))" '("thread"))
    (eval-code-block
     " "
     "(kill-thread spinner)" '("kill-thread"))))
  
  (slide/title/tall/inset
   "Parameters  (a.k.a. Fluid Variables)"
   demo-inset
   (aspect "thread-local state")
   (vl-append
    line-sep
    (eval-code-block
     "(printf \"Hello\\n\")"
     "(fprintf (current-output-port) \"Hola\\n\")" '("current-.*port")
     "(fprintf (current-error-port) \"Goodbye\\n\")" '("current-.*port")
     "(error \"Ciao\")")
    (eval-code-block/sep
     0
     " "
     "(parameterize ((current-error-port (current-output-port)))" '("parameterize")
     "  (error \"Au Revoir\"))")
    (eval-code-block/sep
     0
     " "
     "(parameterize ((current-error-port (current-output-port)))"  '("parameterize")
     "  (thread"  '("thread")
     "    (lambda ()"
     "      (error \"\u518D\u89C1\"))))")))
   
  (slide/title/tall/inset
   "Eventspaces"
   demo-inset
   (aspect "concurrent GUIs")
   (blank)
   (vl-append
    line-sep
    (eval-code-block
     "(thread (lambda () (message-box \"One\" \"Hi\")))"
     "(thread (lambda () (message-box \"Two\" \"Bye\")))")
    (eval-code-block
     " "
     " "
     "(thread (lambda () (message-box \"One\" \"Hi\")))"
     "(parameterize ((current-eventspace (make-eventspace)))" '("(make|current)-eventspace")
     "  (thread (lambda () (message-box \"Two\" \"Bye\"))))")))

  (slide/title/tall/inset
   "Custodians"
   demo-inset
   (aspect "termination and clean-up")
   (blank)
   (vl-append
    line-sep
    (hbl-append
     font-size
     (tt*/normal
      "(define c (make-custodian))" '("make-custodian")
      "(parameterize ((current-custodian c))" '("current-custodian")
      "  ...)")
     (clickback
      eval-link
      (lambda ()
	(end-subtalk)
	(begin-subtalk)
	(subtalk-eval '(define c (make-custodian)))
	(subtalk-eval '(parameterize ((current-custodian c))
			 (parameterize ((current-eventspace (make-eventspace)))
			   (dynamic-require "start-a-lot.scm" #f)))))))
    (eval-code-block
     " "
     "(custodian-shutdown-all c)" '("cust.*all"))))

  #;
  (slide/title/tall/inset
   "Custodians"
   demo-inset
   (aspect "resource limits")
   (blank)
   (vl-append
    line-sep
    (hbl-append
     font-size
     (tt*/normal
      "(define (run-away)"
      "  (cons 1 (run-away)))"
      " "
      "(custodian-limit-memory c 1000000 c)"  '("custodian-limit-memory")
      " "
      "(parameterize ((current-custodian c))"
      "  ..."
      "  (thread run-away))")
     (clickback
      eval-link
      (lambda ()
	(end-subtalk)
	(begin-subtalk)
	(subtalk-eval '(define c (make-custodian)))
	(subtalk-eval '(begin
			 (custodian-limit-memory c 1000000 c)
			 (parameterize ((current-custodian c))
			   (parameterize ((current-eventspace (make-eventspace)))
			     (dynamic-require "start-a-lot.scm" #f)
			     (thread
			      (lambda ()
				(let loop ([n 0]) 
				  (unless (= n 500000) 
				    (add1 (loop (add1 n))))))))))))))))
   

  (slide/title
   "Etc."
   (half-page-item "Security Guards")
   (aspect "resource access control")
   (half-page-item "Namespaces")
   (aspect "global bindings")
   (half-page-item "Will Executors")
   (aspect "timing of finalizations")
   (half-page-item "Inspectors")
   (aspect "debugging access"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title/center
   "Building a Programming Environment"
   (page-para*
    (clickback
     (make-link "SchemeEsq")
     (lambda ()
       (end-subtalk)
       (begin-subtalk)
       (subtalk-eval '(require "scheme-esq.scm"))))
    ", a mini DrScheme"
    (blank font-size)
    (colorize (t "[ICFP 99]") dim-color)))

  (demo-slide/title
   "GUI - Frame"
   (lambda ()
     (end-subtalk)
     (begin-subtalk)
     (subtalk-eval 
      '(begin 
	 (define frame (new frame% [label "SchemeEsq"] [width 400] [height 185]))
	 (send frame show #t))))
   (tt*/normal
    "(define frame"
    "  (new frame% " '("frame%")
    "    [label \"SchemeEsq\"]"
    "    [width 400] [height 175]))"
    ""
    "(send frame show #t)"))

  (eval-code-slide
   "GUI - Reset Button"
   "(new button%" '("button%")
   "  [label \"Reset\"]"
   "  [parent frame]"
   "  [callback (lambda (b e) (reset-program))])")

  (eval-code-slide
   "GUI - Interaction Area"
   "(define repl-display-canvas"
   "  (new editor-canvas%" '("editor-canvas%")
   "    [parent frame]))")

  (demo-slide/title
   "GUI - Interaction Buffer"
   (lambda ()
     (subtalk-eval '(begin
		      (load "text.ss")
		      (define repl-editor (make-object esq-text%))
		      (send repl-display-canvas set-editor repl-editor))))
   (tt*/normal
    "(define esq-text% "
    "  (class text% ... (evaluate str) ...))" '("text%")
    " "
    "(define repl-editor (new esq-text%))"
    "(send repl-display-canvas set-editor repl-editor)"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (eval-code-slide
   "Evaluator"
   "(define (evaluate expr-str)"
   "  (thread" '("thread")
   "    (lambda ()"
   "      (print (eval (read (open-input-string expr-str))))" '("(read)|(eval)|(print)")
   "      (newline)"
   "      (send repl-editor new-prompt))))")

  (demo-slide/title
   "Evaluator Output"
   (lambda ()
     (subtalk-eval
      '(begin
	 (define user-output-port
	   (make-output-port
	    'stdout
            always-evt
	    (lambda (s start end nonblock? w/break?) 
	      (send repl-editor output (bytes->string/utf-8 (subbytes s start end)))
	      (- end start))
	    void))

	 (define (evaluate expr-str)
	   (thread
	    (lambda ()
	      (current-output-port user-output-port)
	      (with-handlers ((exn? 
			       (lambda (exn)
				 (display (exn-message exn)))))
		(print (eval (read (open-input-string expr-str)))))
	      (newline)
	      (send repl-editor new-prompt)))))))
   (tt*/dim
      "(define user-output-port" '<<
      "  (make-output-port ... repl-editor ...))" '<<
      " "
      "(define (evaluate expr-str)"
      "  (parameterize ((current-output-port user-output-port))" '<<
      "    (thread"
      "      (lambda ()"
      "        ...))))" '+2))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (demo-slide/title
   "Evaluating GUIs"
   (lambda ()
     (subtalk-eval
      '(begin
	 (define user-eventspace (make-eventspace))
	 
	 (define (evaluate expr-str)
	   (thread
	    (lambda ()
	      (current-output-port user-output-port)
	      (current-eventspace user-eventspace)
	      (with-handlers ((exn? 
			       (lambda (exn)
				 (display (exn-message exn)))))
		(print (eval (read (open-input-string expr-str)))))
	      (newline)
	      (send repl-editor new-prompt)))))))
   (vl-append
    (tt*/dim
     "(define user-eventspace (make-eventspace))" '<<
     " "
     "(define (evaluate expr-str)"
     "  (parameterize ((current-output-port user-output-port)"
     "                 (current-eventspace user-eventspace))" -1
     "    (thread"
     "      (lambda ()"
     "        ...)))")))

  (demo-slide/title
   "Custodian for Evaluation"
   (lambda ()
     (subtalk-eval
      '(begin
	 (define user-custodian (make-custodian))
	 (define user-eventspace
	   (parameterize ((current-custodian user-custodian))
	     (make-eventspace)))
	 (define (evaluate expr-str)
	   (parameterize ((current-custodian user-custodian))
	     (thread
	      (lambda ()
		(current-output-port user-output-port)
		(current-eventspace user-eventspace)
		(with-handlers ((exn? 
				 (lambda (exn)
				   (display (exn-message exn)))))
		  (print (eval (read (open-input-string expr-str)))))
		(newline)
		(send repl-editor new-prompt))))))))
   (tt*/dim
    "(define user-custodian (make-custodian))" '<<
    " "
    "(define user-eventspace" '<<
    "  (parameterize ((current-custodian user-custodian))" '<<
    "    (make-eventspace)))" '<<
    " "
    "(define (evaluate expr-str)" 
    "  (parameterize ((current-output-port user-output-port)"
    "                 (current-eventspace user-eventspace)"
    "                 (current-custodian user-custodian))" -1
    "    (thread"
    "      (lambda ()"
    "        ...))))"))

  (eval-code-slide
   "Reset Evaluation"
   "(define (reset-program)"
   "  (custodian-shutdown-all user-custodian)"
   " "
   "  (set! user-custodian (make-custodian))"
   "  (parameterize ((current-custodian user-custodian))"
   "    (set! user-eventspace (make-eventspace)))"
   "  (send repl-editor reset))")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  #|
  
  (require "../killsafetalk/squiggle.ss")
  
  (define (scale-process p) (scale p 4))
  
  (slide/title/center
   "Recap: Unix"
   (scale-process unix))

  (slide/title/center
   "Recap: Lisp Machine"
   (scale-process jvm))
  
  (slide/title/center
   "Recap: PLT Scheme"
   (scale-process mz))

  |#
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (outline 'accounting)

  (map
   (lambda (s)
     (slide/title/center
      "Resource Consumption"
      (mk-drs-layout s)))
   '(1 1.25 1.5 2 3 4))

  (with-steps (unix mz gc)
    (slide/title
     "Resource Accounting"
     (cc-superimpose
      ((vonly unix)
       (page-item (colorize (bt "Conventional OS") "red") ": process memory use = size of partition"))
      ((vonly mz)
       (page-item (colorize (bt "Language as OS") "blue") ": process memory use = size of owned data"))
      ((vonly gc)
       (page-para "Our strategy: compute accounting charges during GC")))
     (blank)
     (cc-superimpose
      ((vonly unix)
       unix-memory-icon)
      ((vafter mz) memory-icon))
     (blank)
     (cc-superimpose
      ((vonly unix)
       (vc-append
        gap-size
        (page-subitem "Accounting is easy")
        (page-subitem "Trading data is difficult")))
      ((vonly mz)
       (vc-append
        gap-size
        (page-subitem "Trading data is easy")
        (page-subitem "Accounting" (it "appears") "difficult: sharing, real-time tracking")))
      ((vonly gc)
       (vc-append
	gap-size
	(page-para "See also [Price03]")
	#;
	(colorize
	 (page-para* "Exact accounting is"
                    (hbl-append (t "O(N") (text "2" `(superscript . , main-font) font-size) (t ")"))
                    "in the worst case...")
	 "red"))))))
  
  (require "accounting.ss")

  (define ((owner +a?) s)
    (if (memq s '(z s))
        `(,(if +a? "B, A" "B") ,(symbol->string s) #f)
        `("A" ,(symbol->string s) #t)))
  
  (with-steps (unknown known link)
    (slide/title/center 
     "Basic Accounting"
     (make-basic-picture 
      '(A B) 
      '(1 2 3)
      `(,(map (before known values (owner (after? link))) '(x y z))
        ,(map (before known values (owner (after? link))) '(q r s)))
      `((A 1) (A 2) (B 3) 
              ,@(after link '((B A)) null) 
              (1 x) (2 y) (3 z) (x q) (y r) (z s)))))

  (define (mk-sharing-picture known? child?)
    (make-basic-picture 
     '(A B)
     '(1 2) 
     (if known?
         `((["A" "x" #t] 
            ["B" "y" #f])
           ([,(if child? "A" "A or B") "z" #t]))
         '((x y) (z)))
     `((A 1) (B 2) (1 x) (2 y) (x z) (y z)
             ,@(if child? '((B A)) '()))))
  
  (with-steps (unknown both child)
    (slide/title/center 
     (after child "Sharing: Charge the Parent" "Sharing")
     (mk-sharing-picture (after? both) (after? child))))
     
  (with-steps (unknown known cust weak)
    (slide/title/center 
     "Threads, Custodians, and Weak References"
     (make-basic-picture 
      '(A B) 
      '(1 2) 
      (before known '((x)) '((("B" "x" #t))))
      `((A 1) (B 2) ,(after weak `(1 x !) `(1 ,(after cust 'B 2))) (2 x)))))

  (slide/title
   "Why Charge the Parent?"
   (blank)
   (blank)
   (mk-sharing-picture #t #t)
   (blank)
   (blank)
   (blank)
   (blank)
   'next
   (page-item "Parent is responsible for children")
   'next
   (page-item "Children refer to parent, so if the parent refers to children data directly,"
              " any child is charged for all children"))
  
  (define (full-strikeout image)
    (let ([w (pict-width image)]
          [h (pict-height image)])
      (dc (lambda (dc dx dy)
            (let ([pen (send dc get-pen)]
                  [pictdraw (make-pict-drawer image)])
              (pictdraw dc dx dy)
              
              (send dc set-pen (make-object pen% (make-object color% "Red")
                                 10 'solid))
              (send dc draw-line dx dy (+ w dx) (+ h dy))
              (send dc draw-line (+ dx w) dy dx (+ dy h))
              
              (send dc set-pen pen)))
          w h 0 0)))
  
  (define (strikeout image)
    (let ([w (pict-width image)]
          [h (pict-height image)])
      (cb-superimpose
       image
       (inset (full-strikeout (blank w (* 2/5 h))) 0 0 0 (* 1/10 h)))))
  
  (define dscheme (scale (bitmap "loop.png") 0.45))
  (define dscheme-busy (scale (bitmap "deep.png") 0.45))

  (define running-lbl (colorize (bt "Bad Loop") "Red"))
  (define normal-lbl (colorize (bt "Normal") "Blue"))
  (define killed-lbl (colorize (bt "Shut Down") "Red"))

  (define (drs-slide init? so1 so2 l1 l3)
    (slide/title/center 
     (string-append (if init? "Initial " "Current ") "Experience: DrScheme")
     (hc-append 40 
                (vc-append 20 (so1 dscheme-busy) l1)
                (vc-append 20 dscheme normal-lbl)
                (vc-append 20 (so2 dscheme) l3))))
  
  (drs-slide #t values values running-lbl normal-lbl)
  (drs-slide #t values strikeout running-lbl killed-lbl)
  
  (define (inset-i p) (inset p 20 0 0 0))
  
  (with-steps (strong weak explain)
    (slide/title
     (before weak "DrScheme Bug" "DrScheme Repair")
     (blank)
     (make-basic-picture 
      '(DrScheme User1 User2)
      '(0 1 2)
      '((x _ _)
        (_ y z))
      `((DrScheme 0) 
        (User1 1) 
        (User2 2)
        (0 x) (1 x) (2 x)
        (1 y) (2 z)
        (x y ,@(after weak '(!) '())) 
        (x z ,@(after weak '(!) '()))))
     (blank)
     ((vafter explain)
      (vl-append
       (* 2 line-sep)
       (page-para* "Changed 5 references:")
       (inset-i (page-item* "Weakened 2"))
       (inset-i (page-item* "Removed 2"))
       (inset-i (page-item* "Moved 1 into child"))))))
  
  (drs-slide #f values values running-lbl normal-lbl)
  (drs-slide #f strikeout values killed-lbl normal-lbl)

  (slide/title/center
   "Accounting without Partitions"
   (vl-append
    gap-size
    (page-para* "Useful accounting")
    (page-item* "Doesn't need partitions")
    (page-item* "Does need hierarchy")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (when kill-safe-section?
    (outline 'kill-safe))
  (require "ks.ss")
  (when kill-safe-section?
    (kill-safe-slides))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Conclusion"
   ((if long-conclusion? (lambda (p) (scale p 0.6)) values)
    (vc-append
     font-size
     (hc-append
      the-scheme-angel-file
      (ghost dircomm)
      the-scheme-angel-file)
     together-arrows
     mred-logo))
   (if long-conclusion?
       (vc-append
        gap-size
        (page-item "Programmers need OS-like constructs in languages")
        (vc-append
         (* 4 line-sep)
         (page-subitem "concurrency")
         (page-subitem "adjust run-time environment")
         (page-subitem "easy termination"))
        (blank)
        (page-item "Multiple language constructs for \u201Cprocess\u201D")
        (page-subitem "programmer can mix and match to"
                      "balance isolation and cooperation"))
       (vc-append
        (* 4 line-sep)
        (page-para "But don\u2019t partition data:")
        (page-subitem "closures")
        (page-subitem "objects")
        (page-subitem "continuations")
        (page-subitem "..."))))
  
  'done)
