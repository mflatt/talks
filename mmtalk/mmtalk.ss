(module mmtalk (lib "slideshow-run.ss" "texpict")
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "etc.ss")
           (lib "math.ss")
	   (lib "face.ss" "texpict")
	   "alg.ss"
           (lib "balloon.ss" "texpict"))

  (define require-color "forest green")
  (define require-for-syntax-color "purple")

  (define (bget base-name)
    (make-object bitmap% (build-path (this-expression-source-directory)
				     (format "~a.bmp" base-name))))

  (define emacs-scheme (bitmap (bget "emacs-scheme")))
  (define emacs-macros (bitmap (bget "emacs-macros")))
  (define emacs-mzc (bitmap (bget "emacs-mzc")))
  (define emacs-eval-when (bitmap (bget "emacs-eval-when")))
  (define emacs-gambit (bitmap (bget "emacs-gambit")))
  (define emacs-lib (bitmap (bget "emacs-lib")))
  
  (define (face-slide title face screen)
    (slide/title/center 
     title
     (hc-append 
      (* 3 font-size)
      face
      screen)))

  ;; ----------------------------------------

  (slide/center
   (vc-append
    line-sep
    (titlet "Composable and Compilable Macros"))
   (blank)
   (bitmap (build-path (collection-path "icons") "plt.gif"))
   (blank)
   (vc-append
    line-sep
    (colorize (t "Matthew Flatt") "blue")
    (blank font-size)
    (t "University of Utah")))

  ;; ----------------------------------------

  
  (face-slide "A Scheme Programmer..." (face 'happy) (ghost emacs-scheme))
  (face-slide "... Programming in Scheme" (face 'happier) emacs-scheme)
  (face-slide "... in Macro-Extended Scheme!" (face 'happiest) emacs-macros)
  (face-slide "... Trying to Use a Compiler" (face 'embarassed) emacs-mzc)
  (face-slide "... Accomodating the Compiler" (face 'sortof-happy) emacs-eval-when)
  (face-slide "... Trying a Different Compiler" (face 'sortof-unhappy) emacs-gambit)
  (face-slide "... Trying a Complex Library" (face 'badly-embarassed) emacs-lib)
  
  ;; ----------------------------------------

  (define vfactor 1.2)

  (define mk-file
    (opt-lambda (content [color #t] [vfactor vfactor] [border-color "black"])
      (cc-superimpose
       (file-icon (+ (pict-width content) font-size)
		  (+ (* vfactor (pict-height content)) font-size)
		  color
                  #t)
       content)))

  (define (mk-load name indent?)
    (hbl-append (if indent? (tt "  (") (tt "("))
		(bt "load")
		(tt " ")
		(t (format "\"~a.scm\"" name))
		(tt (if indent? "))" ")"))))

  (define load-a-expr (mk-load "a" #f))
  (define load-b-expr (mk-load "b" #f))
  (define load-m-expr (mk-load "m" #t))
  (define load-u-expr (mk-load "u" #f))
  (define load-u-expr2 (launder load-u-expr))
  (define load-ma-expr (mk-load "ma" #f))
  (define load-mb-expr (mk-load "mb" #f))

  (define orig-file
    (mk-file 
     (vl-append
      line-sep
      load-u-expr2
      (hbl-append (tt "(")
		  (bt "eval-when"))
      (hbl-append (tt "  (")
                  (t "compile load")
                  (tt ")"))
      load-m-expr
      load-a-expr
      load-b-expr
      (tt "..."))))

  (define load-color "blue")

  (define (file-label name)
    (colorize (t name) load-color))
  
  (define (mk-small-content name)
     (vl-append
      line-sep
      (file-label name)
      (ghost (tt "    "))))
  
  (define (mk-m-file u?)
    (mk-file 
     (vl-append
      line-sep
      (if u?
          (colorize load-u-expr "red")
          (hbl-append (tt "") (file-label "m")))
      load-ma-expr
      load-mb-expr)))

  (define a-file (mk-file (mk-small-content "a")))
  (define b-file (mk-file (mk-small-content "b")))

  (define u-file (mk-file (mk-small-content "u")))
  (define ma-file (mk-file (mk-small-content "ma")))
  (define mb-file (mk-file (mk-small-content "mb")))

  (define connect-loads
    (case-lambda
     [(p color find-ref find-mod) p]
     [(p color find-ref find-mod from to . rest)
      (apply connect-loads
	     (add-arrow-line 
	      (/ font-size 2)
	      p
	      from find-ref
	      to find-mod
	      3 color)
	     color find-ref find-mod
	     rest)]))
  
  (define small-face-scale 1/3)
  (define small-unhappy (scale (face 'unhappy) small-face-scale small-face-scale))
  (define small-happy (scale (face 'happy) small-face-scale small-face-scale))
  
  (define loaded-none
    (wrap-balloon (hc-append
                   font-size
                   small-unhappy
                   (vl-append
                    line-sep
                    (t "never loaded")
                    (t "in compile mode")))
                  'e 30 0))
  
  (define loaded-twice
    (wrap-balloon (hc-append
                   font-size
                   small-unhappy
                   (vl-append
                    line-sep
                    (t "loaded twice")
                    (t "in interactive mode")))
                  'ne 30 -50))
  
  (define (demo-arrow color)
    (let ([p (blank (* 2 font-size) font-size)])
      (connect-loads p color find-lc find-rc p p)))
  
  (define (mk-script-ex deps? deps2? no-load? m-to-u? x2?)
    (define m-file (mk-m-file m-to-u?))
    (slide/title/tall
     "The Trouble with Scripting the Compiler"
     (let* ([lc-color (if deps?
                          (scale-color 4.0 load-color)
                          load-color)]
            [p (connect-loads
                (connect-loads
                 (connect-loads
                  (ht-append
                   (* 3 font-size)
                   orig-file
                   (vl-append
                    (* 1.5 font-size)
                    (blank)
                    (blank)
                    m-file
                    a-file
                    b-file)
                   (vl-append
                    (* 2 font-size)
                    u-file
                    ma-file
                    mb-file))
                  lc-color
                  find-rc find-lt
                  load-a-expr a-file
                  load-b-expr b-file
                  load-ma-expr ma-file
                  load-mb-expr mb-file)
                 lc-color
                 find-rc find-lc
                 load-m-expr m-file)
                (if x2? load-color lc-color)
                find-rc find-lt
                load-u-expr2 u-file)]
            [p (if m-to-u?
                   (connect-loads p load-color find-rc find-lc load-u-expr u-file)
                   p)]
            [p (if (and deps? (not x2?))
                   (if deps2?
                       (connect-loads p require-color
                                      find-ct find-cb
                                      ma-file u-file)
                       (connect-loads
                        (connect-loads
                         (connect-loads p require-color
                                        find-ct find-cb
                                        b-file a-file
                                        mb-file ma-file)
                         require-color find-ct find-lb
                         a-file m-file)
                        require-color find-ct find-lc
                        a-file u-file))
                   p)]
            [p (if x2?
                   (place-balloon loaded-twice p u-file find-cb)
                   p)]
            [p (if no-load?
                   (place-balloon loaded-none p u-file find-lc)
                   p)]
            [p (if (not deps?)
                   (rb-superimpose
                    p
                    (hc-append (demo-arrow load-color) (t " = load effect")))
                   p)]
            [p (if (and deps? (not deps2?))
                   (rb-superimpose
                    p
                    (hc-append (demo-arrow require-color) (t " = dependency")))
                   p)])            
       p)))

  (mk-script-ex #f #f #f #f #f)
  (mk-script-ex #t #f #f #f #f)
  (mk-script-ex #t #t #f #f #f)
  (mk-script-ex #t #t #t #f #f)
  (mk-script-ex #t #t #f #t #f)
  (mk-script-ex #t #t #f #t #t)
    
  ;; ----------------------------------------

  (define outline
    (make-outline 
     'problem "Problem" null
     'solution "Solution" (lambda (which)
			    (vl-append
			     (/ font-size 2)
			     (page-item* "Declarative")
			     (page-item* "Consistent success/failure in all compilation modes")
			     (page-item* "Supports")
			     (page-subitem* "lexically scoped macros")
			     (page-subitem* "macro-defining macros")))
     'example "Example" (lambda (which)
			  (page-para (inset (page-item* "Record matching with static checks") (* 5 font-size) 0)))
     'experience "Experience"
     (lambda (which)
       (page-para (inset (page-item* "The many languages & compilers of PLT Scheme") (* 5 font-size) 0)))))
  
  (outline 'solution)

  (define pale-require-color (scale-color #e3 (make-object color% require-color)))
  (define pale-require-for-syntax-color (scale-color #e3 (make-object color% require-for-syntax-color)))

  (define (mk-req name for-syntax?)
    (colorize (alg-code (format "~~(~a \"~a.scm\")"
				(if for-syntax? "require-for-syntax" "require")
				name))
	      (if for-syntax?
		  require-for-syntax-color
		  require-color)))

  (define (cloud-bg color)
    (lambda (p) 
      (let ([d (pict-descent p)])
	(lbl-superimpose
	 (lift (cloud (pict-width p) (pict-height p) color)
	       (- d))
	 (lift (inset (colorize p "white") -1 0 0 -1) (- 1))
	 p))))
		
  (define (mk-a-module name vfactor add-end? body-color file-color border-color . lines)
    (mk-file
     (apply
      vl-append
      line-sep
      (alg-code (format "(module ~a" name))
      (append
       lines
       (if add-end?
	   (list (hbl-append
		  (alg-code "~") 
		  ((if body-color (cloud-bg body-color) values) (tt "..."))
		  (alg-code ")")))
	   null)))
     file-color
     vfactor
     border-color))

  (define (mk-module name body-color . lines)
    (apply mk-a-module name vfactor #t body-color #t "black" lines))

  (define (find-3/4t in p)
    (let-values ([(x y) (find-ct in p)])
      (values (+ x (* 1/4 (pict-width p))) y)))

  (define rt-blob ((cloud-bg require-color) (ghost (alg-code "..."))))
  (define ct-blob ((cloud-bg require-for-syntax-color) (ghost (alg-code "..."))))
  
  (define overview-steps
    '(a a-rt a-ct a-ctx b b-rt b-ct u au split))

  (define (mk-modules step)
    (define (after p)
      (memq step (or (memq p overview-steps) null)))
    (define (between p1 p2)
      (and (after p1) (or (eq? step p2) (not (after p2)))))
    (define (between-excl p1 p2)
      (and (after p1) (not (after p2))))
    
    (define (vafter p)
      (if (after p)
	  values
	  ghost))

    (define require-u (mk-req "u" #f))
    (define require-a (mk-req "a" #f))
    (define require-for-syntax-u (mk-req "u" #t))

    (define mk-define
      (opt-lambda (name stx? color? [both? #f])
        (let ([rhs ((if color?
                        (cloud-bg (if stx?
                                      require-for-syntax-color
                                      require-color))
                        values)
                    (alg-code "..."))])
          (let ([def (hbl-append
                      (alg-code "~(")
                      (alg-code (if stx? "define-syntax" "define"))
                      (colorize (alg-code (format " ~a " name)) require-color)
                      rhs
                      (alg-code ")"))])
            (if both?
                (values def rhs)
                def)))))
    
    (define b-module
      (mk-module
       "b" (and (after 'b-rt) require-color)
       require-a
       ((vafter 'u) require-for-syntax-u)
       (mk-define "n" #t (after 'b-ct))
       (mk-define "g" #f (after 'b-rt))))
    
    (define a-rt? (after 'a-rt))
    
    (define-values (a-mod-macro-def a-mod-macro-body)
      (mk-define "m" #t (after 'a-ct) #t))
    
    (define a-module
      (apply
       mk-module
       "a" (and a-rt? require-color)
       (append
        (if (after 'au)
            (list require-u)
            null)
        (list
         a-mod-macro-def
         (mk-define "f" #f a-rt?)))))

    (define a-balloon
      (wrap-balloon
       (hbl-append (alg-code "#'")
                   ((cloud-bg require-color) (alg-code "(f ...)")))
       'ne 20 -70))       
       
    (define (mk-u-module body-color)
      (mk-module
       "u" #f
       (mk-define "fold" #f #f)))
    
    (define u-module (mk-u-module
		      (if (after 'split)
			  (or #t pale-require-color)
			  #t)))
    (define u-split-module 
      (inset (if (after 'split)
		 (mk-u-module (or #t pale-require-for-syntax-color))
		 (ghost (launder u-module)))
	     font-size font-size
	     0 0))

    (define (inset-blurb p)
      (inset p 0 font-size 0 0))

    (define p
      (vc-append
       (* 2 font-size)
       (lt-superimpose
        u-split-module
        ((vafter 'u) u-module))
       (ht-append
        (* 2 font-size)
        a-module
        ((vafter 'b) b-module))))
    
    (slide/title/center
     "Modules and Macros"
     (let* ([p (if (after 'b)
		   (connect-loads
		    p require-color find-lc find-rc
		    require-a a-module)
		   p)]
	    [p (if (after 'u)
		   (connect-loads
		    p require-for-syntax-color find-3/4t find-rb
		    require-for-syntax-u (if (after 'split)
					     u-split-module 
					     u-module))
		   p)]
	    [p (if (after 'au)
		   (connect-loads
		    p require-color find-3/4t find-lb
		    require-u u-module)
		   p)]
            [p (if (between-excl 'a-ctx 'au)
                   (place-balloon a-balloon p a-mod-macro-body find-cb)
                   p)]
            [p (if (between 'a-rt 'a-rt)
                   (lt-superimpose
		    (inset-blurb
		     (hbl-append rt-blob (t " = run-time expressions")))
                    p)
                   p)]
            [p (if (between 'a-ct 'a-ct)
                   (lt-superimpose
		    (inset-blurb
		     (hbl-append ct-blob (t " = compile-time expressions")))
                    p)
                   p)]
            [p (if (between 'a-ctx 'a-ctx)
                   (lt-superimpose
		    (inset-blurb
		     (hbl-append (alg-code "#'") (t " in ") ct-blob
				 (t " escapes back to ") rt-blob))
                    p)
                   p)]
            [p (if (between 'split 'split)
                   (place-balloon
                    (wrap-balloon
                     (vl-append
                      line-sep
                      (t "different instance")
                      (t "for each phase"))
                     'sw -30 0)
                    p u-split-module find-rc)
                   p)])
       p)))

  (map mk-modules overview-steps)

  ;; ----------------------------------------

  (outline 'example)

  (define recmatch-steps
    '(zebra zebra-ex lizard match weight
	    extra-field comp-time))

  (define (mk-record-ex step)
    (define (after p)
      (memq step (or (memq p recmatch-steps) null)))
    (define (vafter what) (if (after what) values ghost))

    (define (equals expr val)
      (hbl-append expr
		  (tt " ; = ")
		  val))

    (define separator
      (cc-superimpose
       (tt " ")
       (colorize (hline (* client-w 2/3) 1) "blue")))

    (define zebra-def
      (alg-code "(define-record zebra (weight stripes))"))
    (define record-switch
      (alg-code
       (format "~~(record-switch ~a" (if (after 'weight) "a" "..."))))
    (define after-code (alg-code ""))
    
    (define p
      (vl-append
       line-sep
       zebra-def
       (alg-code "")
       ((vafter 'zebra-ex)
	(equals (alg-code "(zebra 500 24)")
		(t "a zebra instance")))
       ((vafter 'lizard)
	(vl-append
	 line-sep
	 separator
	 (alg-code "(define-record lizard (weight length color))")
	 (alg-code "")
	 (equals (alg-code "(lizard 2 5 'green)")
		 (t "a lizard instance"))))
       ((vafter 'match)
	(vl-append
	 line-sep
	 separator
	 ((vafter 'weight) (alg-code "(define (animal-weight a)"))
	 record-switch
	 (hbl-append
	  (alg-code
	   (format "~~~~((zebra w s~a) ~a)" 
		   (if (after 'extra-field) " !c" "")
		   (if (after 'weight) "w" "...")))
	  ((vafter 'extra-field)
	   (hbl-append (tt "   ") (colorize (bt "syntax error") "red"))))
	 (alg-code
	  (format "~~~~((lizard w l c) ~a))"  (if (after 'weight) "w)" "...")))
	 after-code
	 ((if (eq? step 'weight) values ghost)
	  (alg-code*
	   (equals (alg-code "(animal-weight (zebra 500 24))")
		   (alg-code "500"))
	   (equals (alg-code "(animal-weight (lizard 2 5 'green))")
		   (alg-code "2"))))))))

    (let ([p (if (after 'comp-time)
		 (let-values ([(tx ty) (find-lb p zebra-def)]
			      [(bx by) (find-lt p record-switch)]
			      [(ax ay) (find-lc p after-code)]
			      [(h) (pict-height p)]
			      [(ah) (arrowhead (* 2 font-size) (- (/ pi 4)))])
		   (cb-superimpose
		    (cons-picture 
		     p
		     `((place ,(- bx (* 0.4 (pict-width ah))) ,(- by font-size) ,(colorize ah require-for-syntax-color))
		       (place 0 0 ,(dc (lambda (dc x y)
					 (define p (send dc get-pen))
					 (send dc set-pen 
					       (send the-pen-list find-or-create-pen require-for-syntax-color (/ font-size 2) 'solid))
					 (send dc draw-spline 
					       (+ x tx) (+ y (- h ty))
					       (+ x (- tx (* 3 font-size))) (+ y (- h (/ (+ ty by) 2)))
					       (+ x bx) (+ y (- h by)))
					 (send dc set-pen p))
				       10 h 0 0))))
		    (colorize
		     (vc-append
		      line-sep
		      (bt "compile-time communication")
		      ((vafter 'comp-time)
		       (bt "across module boundaries")))
		     require-for-syntax-color)))
		 p)])
      (slide/title/tall
       "Records and Matching"
       p)))

  (map mk-record-ex recmatch-steps)

  (define light-scale 3)
  
  (define file-icon-color
    (make-object color% 200 200 255))
  
  (define light-black (scale-color light-scale "black"))
  (define light-file-color (scale-color light-scale file-icon-color))
  
  (define (mk-pmodule name light? . lines)
    (let ([p (apply mk-a-module name 1.0 #f #f 
                    (or (not light?) light-file-color)
                    (if light? light-black "black")
                    lines)])
      (if light?
          (colorize p light-black)
          p)))
  
  (define (ext s)
    (colorize (t s) "blue"))

  (define example-steps
    '(just-m client defmac client-exp1 client-exp2 defmac-rt
             table table-ct client-ct1 client-ct2 ct-done
             user user-fail mgm mgm-use mgm-done))

  (define (mk-ex-modules step)
    (define (after p)
      (memq step (or (memq p example-steps) null)))
    (define (between p1 p2)
      (and (after p1) (or (eq? step p2) (not (after p2)))))
    (define (between-excl p1 p2)
      (and (after p1) (not (after p2))))
    
    (define (vafter p)
      (if (after p)
	  values
	  ghost))

    (define defmac-defrec-body
      (hbl-append (alg-code "~~..#'(define @name (")
		  ((if (after 'defmac-rt) (cloud-bg require-color) values)
		   (alg-code "mkrec"))
		  (alg-code " ..))..)")))
    (define defmac-switch-body
      (hbl-append (alg-code "~~..#'(")
		  ((if (after 'defmac-rt) (cloud-bg require-color) values)
                   (alg-code "is-a"))
		  (alg-code " ")
		  (alg-code " @name ..) ..)")))

    (define (table-body defrec?)
      (lbl-superimpose
       (ghost (if defrec?
		  defmac-defrec-body
		  defmac-switch-body))
       (hbl-append
	(cond
	 [(between-excl 'table 'ct-done)
	  (hbl-append
	   (alg-code "~~... ") 
	   ((if (after 'table-ct)
		(cloud-bg require-for-syntax-color)
		values)
	    (alg-code "table"))
	   (alg-code " ...)"))]
	 [(between-excl 'defmac 'ct-done)
	  (if defrec?
	      defmac-defrec-body
	      defmac-switch-body)]
         [(and defrec? (after 'mgm))
          (hbl-append (alg-code "~~..#'(for-syntax ..")
                      ((cloud-bg require-for-syntax-color) (alg-code "table"))
                      (alg-code "..)..)"))]
	 [else
	  (hbl-append (alg-code "~~...)"))])
	(if defrec?
	    (t "")
	    (alg-code ")")))))

    (define tab-module
      (mk-pmodule "ct" #f
		  (alg-code* "~(define table ...)"
			     "~...)")))

    (define rt-module
      (mk-pmodule "rt" #f
		  (alg-code* "~(define mkrec ...)"
                             "~(define is-a ...))")))

    (define require-tab
      (alg-code "~(require-for-syntax \"ct.scm\")"))

    (define require-rt
      (alg-code "~(require \"rt.scm\")"))

    (define require-dt
      (alg-code "~(require \"record.scm\")"))

    (define dt-module (mk-pmodule 
		       "record"
                       #f
		       ((vafter 'defmac-rt)
			(colorize
			 require-rt
			 require-color))
		       ((vafter 'table-ct)
			(colorize
			 require-tab
			 require-for-syntax-color))
		       (alg-code
			"~(define-syntax define-record")
		       (table-body #t)
		       (alg-code
			"~(define-syntax record-switch")
		       (table-body #f)))
    
    (define def-rec-use
      (alg-code "~(define-record zebra (wgt sc))"))
    (define switch-use
      (alg-code "~~((zebra w s) ...))"))
    
    (define zoo-module (mk-pmodule
                        "zoo"
                        #f
                        (colorize require-dt require-color)
                        def-rec-use
                        (alg-code "~(record-switch ...")
                        switch-use))

    (define require-zoo
      (alg-code "~(require \"zoo.scm\")"))
    
    (define user-switch-use
      (alg-code "~(record-switch ..."))
    
    (define user-module (mk-pmodule
                         "client"
                         #f
                         (colorize require-zoo require-color)
                         user-switch-use
                         (alg-code "~~((zebra w s) ...)))")))

    (define p (ht-append
	       (* 2 font-size)
	       (vc-append
		(* 2 font-size)
		dt-module
		((vafter 'client) zoo-module))
	       (vc-append
		(* 2 font-size)
		((vafter 'defmac-rt) rt-module)
		((vafter 'table-ct) tab-module)
                (cc-superimpose
                 ((if (between 'ct-done 'ct-done) values ghost)
                  (scale (face 'happy) 3/5 3/5))
                 ((vafter 'user) user-module)))))

    (define (mk-ce1 body) (wrap-balloon body 'sw -30 0))
    (define (mk-ce2 body) (wrap-balloon body 'w -30 0))
    
    (define client-exp1 (mk-ce1 
                         (vl-append
                          (ext "expands to")
                          (hbl-append
                           (alg-code "(define zebra (")
                           ((if (after 'defmac-rt) (cloud-bg require-color) values)
                            (alg-code "mkrec"))
                           (alg-code " ...))")))))
    
    (define client-exp2 (mk-ce2
                         (vl-append
                          (ext "expands to")
                          (hbl-append
                           (alg-code "(")
                           ((if (after 'defmac-rt) (cloud-bg require-color) values)
                            (alg-code "is-a"))
                           (alg-code " zebra ...)")))))

    (define client-ct1 (mk-ce1 
                        (vl-append
                         (hbl-append (ext "adds ") (alg-code "zebra") (ext " to ") (alg-code "table"))
                         (ext "while expanding"))))
          
    (define client-ct2 (mk-ce2
                        (vl-append
                         (hbl-append (ext "finds ") (alg-code "zebra") (ext " in ") (alg-code "table"))
                         (ext "while expanding"))))
          
    (define user-fail (wrap-balloon
                       (hc-append
                        font-size
                        small-unhappy
                        (vl-append
                         (hbl-append (ext "cannot find ") (alg-code "zebra"))
                         (hbl-append (ext " in ") (alg-code "table"))))
                       'ne 30 0))

    (define user-succeed (wrap-balloon
                          (hc-append
                           font-size
                           small-happy
                           (vl-append
                            (hbl-append (ext "finds ") (alg-code "zebra"))
                            (hbl-append (ext " in ") (alg-code "table"))))
                          'ne 30 0))

    (define client-new-exp (wrap-balloon
                            (vl-append
                             (ext "now expands to")
                             (hbl-append
                              (alg-code "(for-syntax ..") 
                              ((cloud-bg require-for-syntax-color) (alg-code "table"))
                              (alg-code "..)"))
                             (alg-code "(define zebra (mkrec ...))"))
                            'sw 0 30))

    (slide/title/center
     "Implementing Records"
     (let* ([p (if (after 'client)
                   (connect-loads
                    p require-color find-3/4t find-cb
                    require-dt dt-module)
                   p)]
            [p (if (after 'defmac-rt)
		   (connect-loads
		    p require-color find-rc find-lc
		    require-rt rt-module)
		   p)]
	    [p (if (after 'table-ct)
		   (connect-loads
		    p require-for-syntax-color find-rc find-lc
		    require-tab tab-module)
		   p)]
            [p (if (after 'user)
		   (connect-loads
		    p require-color find-lc find-rc
		    require-zoo zoo-module)
		   p)]
            [p (if (between 'client-exp1 'defmac-rt)
                   (place-balloon client-exp1 p
                                  def-rec-use find-rc)
                   p)]
            [p (if (between 'client-exp2 'defmac-rt)
                   (place-balloon client-exp2 p
                                  switch-use find-rc)
                   p)]
	    [p (if (between-excl 'client-ct1 'ct-done)
                   (place-balloon client-ct1 p
                                  def-rec-use find-rc)
                   p)]
            [p (if (between-excl 'client-ct2 'ct-done)
                   (place-balloon client-ct2 p
                                  switch-use find-rc)
                   p)]
	    [p (if (between-excl 'user-fail 'mgm)
                   (place-balloon user-fail p
                                  user-switch-use find-lc)
		   p)]
	    [p (if (between 'mgm-use 'mgm-done)
                   (place-balloon client-new-exp p
                                  def-rec-use find-3/4t)
		   p)]
	    [p (if (between 'mgm-done 'mgm-done)
                   (place-balloon user-succeed p
                                  user-switch-use find-lc)
		   p)])
       p)))

  (map mk-ex-modules example-steps)

  ;; ----------------------------------------

  (outline 'experience)

  (define dr-student (bitmap "drscheme-beginner.bmp"))
  (define dr-module (bitmap "drscheme-module.bmp"))
  (define dr-menu (bitmap "drscheme-menu.bmp"))

  (define language-balloon
    (wrap-balloon (vl-append
		   line-sep
		   (t "DrScheme supports multiple")
		   (t "languages that share libraries"))
		  'nw -30 -30))

  (define one-mode
    (wrap-balloon (t "One compilation mode") 'ne 30 -30))
  (define another-mode
    (wrap-balloon (vl-append line-sep (t "Another") (t "compilation mode")) 'ne 30 -30))
  (define yet-another-mode
    (wrap-balloon (vl-append line-sep (t "Yet another") (t "compilation mode")) 'e 30 0))

  (define drscheme-steps
    '(student languages module execute check-syntax make-exec happy!))

  (define (mk-drscheme step)
    (slide/title/center
     "PLT Scheme"
     (hc-append
      font-size
      (let ([base
	     (case step
	       [(student languages) dr-student]
	       [(module execute check-syntax happy!) dr-module]
	       [(make-exec) dr-menu])])
	(let-values ([(balloon x y)
		      (case step
			[(languages) (values language-balloon 300 120)]
			[(execute) (values one-mode 500 385)]
			[(check-syntax) (values another-mode 400 385)]
			[(make-exec) (values yet-another-mode 370 337)]
			[else (values #f #f #f)])])
	  (if balloon
	      (place-balloon balloon base base (lambda (a b) (values x y)))
	      base)))
      ((if (eq? step 'happy!) values ghost)
       (face* 'none 'huge #f default-face-color 0)))))
  
  (map mk-drscheme drscheme-steps)
  
  ;; ----------------------------------------

  (define (ackt s)
    (colorize (bt s) "blue"))

  (slide/title
   "Acknowledgements"
   (page-item (ackt "Modules"))
   (page-subitem "Influenced by practically every implemented module system")
   (page-item (ackt "Macros"))
   (page-subitem "Long Scheme history")
   (page-subitem "Builds directly on Dybvig-Hieb-Bruggeman (1993)")
   (page-item (ackt "Making it work"))
   (page-subitem "PLT members"))

  ;; ----------------------------------------
  
  'done)
