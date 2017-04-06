(module accounting (lib "slideshow.ss" "slideshow")
  (require (lib "balloon.ss" "texpict")
           "../mredtalk2/angel-sequence.ss")
  
  (provide make-basic-picture)
  
  (define (lblt s)
    (scale/improve-new-text (t s) 0.8))
  
  (define (make-custodian-pict name)
    (let ([box (inset
                (color-round-frame
                 (inset (if (= 1 (string-length name))
                            (hbl-append (lblt "custodian ")
                                        (bt name))
                            (bt name))
                        7) 
                 0.25 "Red" 5)
                2)])
      (refocus
       (hc-append (scale (scheme-angel-file) 0.5) (bt "= ") box)
       box)))
  
  (define (make-thread-pict name)
    (inset
     (color-round-frame
      (inset (hbl-append (lblt "thread ") (bt name)) 7)
      10 "Blue" 5)
     2))
  
  (define ((find-xx xp yp) p sub)
    (let-values ([(x y) (find-lt p sub)])
      (values (+ x (* xp (pict-width sub)))
              (- y (* yp (pict-height sub))))))
  
  (define find-above (find-xx 0.6 0.1))
  (define find-below (find-xx 0.6 0.9))
  
  (define (make-object-pict name first?)
    (letrec ([force-full (lambda (name)
			   (cond
			     [(symbol? name) (list "" (symbol->string name)
                                                   first?)]
			     [(null? (cddr name)) (list (car name) (cadr name)
                                                        first?)]
			     [else name]))]
	     [object-description->name (lambda (name)
					 (cadr (force-full name)))]
	     [object-description->set (lambda (name)
					(car (force-full name)))]
	     [object-description->display-bubble? (lambda (name)
						    (not (or (symbol? name)
							     (null?
                                                              (cddr name)))))]
	     [object-description->header? (lambda (name)
					    (and (not (symbol? name))
						 (null? (cddr name))))]
	     [object-description->above? (lambda (name)
					   (caddr (force-full name)))])
      (let* ([str (object-description->name name)]
             [set (object-description->set name)]
             [display-bubble? (object-description->display-bubble? name)]
             [above? (object-description->above? name)]
             [header? (object-description->header? name)]
             [obj ((if (string=? "_" str) ghost values)
                   (let ([p (bt str)])
                     (cc-superimpose
                      (cloud (+ 40 (pict-width p)) (+ 40 (pict-height p)) "forest green")
                      (colorize p "white"))))])
        (cond
          [display-bubble?
           (values (if (string? str) (string->symbol str) str) obj
                   (place-balloon
                    (wrap-balloon (bt set)
                                  (if above? 'sw 'nw)
                                  -10
                                  ((if above? + -) 0)
                                  balloon-color 15)
                    obj obj (if above? find-above find-below)))]
          [header? (let* ([setp (inset (text set '() 24) 7)]
                          [strp (inset (text (format "  ~a  " str) '() 24) 7)]
                          [box (hc-append 0 setp strp)]
                          [box/line (add-line box setp find-rt setp find-rb
                                              2 "Black")]
                          [all (color-round-frame box/line 0 "Purple" 5)])
                     (values (if (string? str) (string->symbol str) str)
                             all all))]
          [else (values (if (string? str) (string->symbol str) str) obj obj)]))))
  
  (define (process-object-info objs)
    (let loop ([ls objs] [pictacc '()] [lookupacc '()])
      (cond
	[(null? ls) (values (reverse pictacc) (reverse lookupacc))]
	[else (let-values ([(name obj pict) (make-object-pict (car ls)
                                                              (null? pictacc))])
		(loop (cdr ls) (cons pict pictacc)
                      (cons (cons name obj) lookupacc)))])))
  
  (define (process-object-info* objs)
    (cond
      [(null? objs) (values '() '())]
      [else
       (let-values ([(opicts1 lookupo1) (process-object-info (car objs))]
                    [(opicts2 lookupo2) (process-object-info* (cdr objs))])
         (values (cons opicts1 opicts2) (append lookupo1 lookupo2)))]))
  
  (define arrow-color "dark gray")
  (define weak-arrow-color "orange")

  (define (check-weak-color l)
    (if (null? (cddr l))
        arrow-color
        weak-arrow-color))
  
  (define (make-basic-picture custodians threads objects connections)
    (let ([cpicts (map make-custodian-pict (map symbol->string custodians))]
	  [tpicts (map make-thread-pict (map number->string threads))])
      (let-values ([(opicts lookupo) (process-object-info* objects)])
	(let ([call (apply vc-append (cons 100 cpicts))]
	      [tall (apply vc-append (cons 100 tpicts))]
	      [oall (apply hc-append (cons 100 (map (lambda (x)
						      (apply vc-append
                                                             (cons 35 x)))
                                                    opicts)))]
	      [lookupc (map cons custodians cpicts)]
	      [lookupto (append (map cons threads tpicts) lookupo)]
	      [offhi (lambda (find)
		       (lambda (pict pict-path)
			 (let-values ([(dx dy) (find pict pict-path)])
			   (values dx (- dy 10)))))]
	      [offlow (lambda (find)
			(lambda (pict pict-path)
			  (let-values ([(dx dy) (find pict pict-path)])
			    (values dx (+ dy 10)))))])
          (let* ([all (hc-append 100 call tall oall)]
                 [arrows
                  (let loop ((ls connections) (all (ghost all)))
                    (cond
                      [(null? ls) all]
                      ;; links from custodian to thread
                      [(and (member (caar ls) custodians) (member (cadar ls) threads))
                       (loop (cdr ls)
                             (add-arrow-line 10 all (cdr (assq (caar ls) lookupc)) find-rc
                                             (cdr (assq (cadar ls) lookupto)) find-lc 2 arrow-color))]
                      ;; links from custodian to custodian
                      [(and (member (caar ls) custodians)
                            (member (cadar ls) custodians))
                       (loop (cdr ls)
                             (add-arrow-line 10 all (cdr (assq (cadar ls) lookupc)) find-cb
                                             (cdr (assq (caar ls) lookupc)) find-ct 2 arrow-color))]
                      ;; links from thread to custodian
                      [(and (member (caar ls) threads) (member (cadar ls) custodians))
                       (loop (cdr ls)
                             (add-arrow-line 10 all (cdr (assq (caar ls) lookupto)) find-cb
                                             (cdr (assq (cadar ls) lookupc)) find-ct 2 arrow-color))]
                      ;; links between threads and threads
                      [(and (member (caar ls) threads) (member (cadar ls) threads))
                       (loop (cdr ls)
                             (add-arrow-line 10 all (cdr (assq (caar ls) lookupto)) find-cb
                                             (cdr (assq (cadar ls) lookupto)) find-ct 2 arrow-color))]
                      ;; links between other things
                      [(member (cddar ls) '(() (!)))
                       (loop (cdr ls)
                             (let ([from (assq (caar ls) lookupto)]
                                   [to (assq (cadar ls) lookupto)])
                               (if (and to from)
                                   (add-arrow-line 10 all (cdr from) find-rc
                                                   (cdr to) find-lc 2 (check-weak-color (car ls)))
                                   all)))]
                      [else
                       (let ([all (add-arrow-line 10 all
                                                  (cdr (assq (caar ls) lookupto)) (offhi find-rc)
                                                  (cdr (assq (cadar ls) lookupto)) (offhi find-lc)
                                                  2 
						  (if (member (cddar ls) '((~)))
						      weak-arrow-color
						      arrow-color))])
                         (loop (cdr ls)
                               (add-arrow-line 10 all (cdr (assq (cadar ls) lookupto))
                                               (offlow find-lc) (cdr (assq (caar ls) lookupto))
                                               (offlow find-rc) 2 arrow-color)))]))])
            (cc-superimpose arrows all)))))))
