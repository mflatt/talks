#lang slideshow
(require slideshow/code
         slideshow/balloon
         slideshow/play
         (for-syntax racket/base)
         racket/class
         racket/draw
         racket/pretty
         "color.rkt"
         (only-in "code.rkt" encolors scope1/text scope2/text))

(provide code-sequence
         code
         scope-example
         QUOTE
         QQUOTE
         sc1
         sc2
         scope-balloon)

(define use-transitions? #t)

(define-syntax-rule (code-sequence e ...)
  (do-code-sequence
   `(e ...)
   (list (quote-syntax e) ...)
   (parameterize ([current-keyword-list
                   (cons "match-define"
                         (remove* '("syntax") (current-keyword-list)))])
     (list (code e) ...))))

(define-syntax QUOTE
  (make-code-transformer
   (lambda (stx)
     (and (identifier? stx)
          #'(typeset-code (quote-syntax quote))))))

(define-syntax QQUOTE
  (make-code-transformer
   (lambda (stx)
     (and (identifier? stx)
          #'(colorize (tt "quote")
                      (current-literal-color))))))

(define-syntax sc1
  (make-code-transformer
   (lambda (stx)
     #'(colorize (tt "sc1") scope1/text))))

(define-syntax sc2
  (make-code-transformer
   (lambda (stx)
     #'(colorize (tt "sc2") scope2/text))))

(struct code-state (title mode accum 
                          code-desc code post-code-desc code-balloons
                          example-def
                          forwards
                          start-here?
                          scale
                          done-height
                          total-height
                          accum-code-height
                          last-emitted
                          keep-code?
                          all-code
                          all-code-test-preamble))

(define (do-code-sequence exprs stxes picts)
  (define ns (make-base-namespace))
  (eval '(require racket/set) ns)
  (eval '(require racket/list) ns)
  (eval '(require racket/match) ns)
  (eval '(define datum->syntax #f) ns)
  (eval '(define syntax->datum #f) ns)
  (eval '(define compile #f) ns)
  (eval '(define expand #f) ns)
  (define tests null)
  (define (add-test! e1 e2)
    (set! tests (cons (lambda ()
                        (define got (eval (clean e1) ns))
                        (define expected (eval (clean e2) ns))
                        (unless (equal? got expected)
                          (error "test failed" e1 e2
                                 got expected)))
                      tests)))
  (let loop ([exprs exprs] [stxes stxes] [picts picts] [state (code-state "Untitled" #f
                                                                          null null null #f
                                                                          null
                                                                          null
                                                                          #hasheq() #f 1
                                                                          0 (count-code-height exprs picts) 0
                                                                          #f
                                                                          #t
                                                                          null
                                                                          null)])
    (cond
     [(null? exprs)
      
      (emit state)]
     [else
      (define kw (car exprs))
      (case kw
        [(#:part)
         (emit state #:done-code? #t)
         (define new-state (struct-copy code-state (emitted state #:done-code? #t)
                                        [mode #f]))
         (parameterize ([current-slide-assembler (make-state-slide-assembler new-state)])
           (slide
            #:name (string-append (cadr exprs) " ====")
            (titlet (cadr exprs))))
         (loop (cddr exprs) (cddr stxes) (cddr picts) new-state)]
        [(#:external #:external/code)
         (emit state #:done-code? #t)
         (define new-state (emitted state #:done-code? #t))
         (parameterize ([current-slide-assembler (make-state-slide-assembler new-state)])
           (case kw
             [(#:external/code)
              ((cadr exprs)
               (lambda (keep-tests?)
                 (gather-all-code (reverse (code-state-all-code state))
                                  (reverse (code-state-all-code-test-preamble state))
                                  #:keep-tests? keep-tests?)))]
             [else ((cadr exprs))]))
         (loop (cddr exprs) (cddr stxes) (cddr picts) new-state)]
        [(#:eval)
         (eval (cadr exprs) ns)
         (loop (cddr exprs) (cddr stxes) (cddr picts)
               (struct-copy code-state state
                            [all-code-test-preamble (cons `(module+ test ,(cadr stxes))
                                                          (code-state-all-code-test-preamble state))]))]
        [(#:eval/forward)
         (eval (caddr exprs) ns)
         (loop (cdddr exprs) (cdddr stxes) (cdddr picts) 
               (struct-copy code-state state
                            [forwards (hash-set (code-state-forwards state)
                                                (cadr exprs)
                                                (cons (caddr stxes)
                                                      (at-scale state (caddr picts))))]))]
        [(#:eval-example)
         (loop (cdddr exprs) (cdddr stxes) (cdddr picts)
               (struct-copy code-state state
                            [all-code (cons `(module+ test
                                              (check-equal? ,(cadr stxes)
                                                            ,(caddr stxes)))
                                            (code-state-all-code state))]))
         (add-test! (cadr exprs) (caddr exprs))]
        [(#:code #:temporary-code)
         (emit state #:done-code? #t)
         (define keep? (not (eq? (car exprs) '#:temporary-code)))
         (loop (cdr exprs) (cdr stxes) (cdr picts) (struct-copy code-state (emitted state #:done-code? #t)
                                                                [keep-code? keep?]
                                                                [mode '#:code]
                                                                [all-code 
                                                                 (if keep?
                                                                     (cons #'code:blank
                                                                           (code-state-all-code state))
                                                                     (code-state-all-code state))]))]
        [(#:code-balloon #:code-balloon+ #:code-balloon/- #:code-balloon/-- #:code-balloon/+-)
         (emit state)
         (loop (cddr exprs) (cddr stxes) (cddr picts)
               (struct-copy code-state (emitted state)
                            [code-balloons (cons (cadr exprs)
                                                 (case (car exprs)
                                                   [(#:code-balloon) null]
                                                   [(#:code-balloon/-) 
                                                    (cdr (code-state-code-balloons state))]
                                                   [(#:code-balloon/+-) 
                                                    (cons (car (code-state-code-balloons state))
                                                          (cddr (code-state-code-balloons state)))]
                                                   [(#:code-balloon/--) 
                                                    (cddr (code-state-code-balloons state))]
                                                   [else
                                                    (code-state-code-balloons state)]))]
                            [scale (code-state-scale state)]
                            [mode '#:code]))]
        [(#:example #:example/left)
         (emit state)
         (loop (cdr exprs) (cdr stxes) (cdr picts) (struct-copy code-state (emitted state)
                                                                [mode (car exprs)]))]
        [(#:section)
         (emit state)
         (loop (cddr exprs) (cddr stxes) (cddr picts) (struct-copy code-state (emitted state)
                                                                   [title (cadr exprs)]
                                                                   [mode #f]))]
        [(#:example-def)
         (emit state)
         (loop (cdr exprs) (cdr stxes) (cdr picts) (struct-copy code-state (emitted state)
                                                                [mode '#:example-def]
                                                                [example-def null]))]
        [(#:start-here)
         (emit state)
         (loop (cdr exprs) (cdr stxes) (cdr picts) (struct-copy code-state (emitted state)
                                                                [start-here? #t]))]
        [(#:next)
         (emit state #:force? #t)
         (loop (cdr exprs) (cdr stxes) (cdr picts) (emitted state #:force? #t))]
        [(#:scale)
         (loop (cddr exprs) (cddr stxes) (cddr picts) (struct-copy code-state state
                                                                   [scale (cadr exprs)]))]
        [else
         (when (keyword? (car exprs)) (error "unrecognized keyword" (car exprs)))
         (case (code-state-mode state)
           [(#:code)
            (define forward-e+stx
              (and (pair? (car exprs))
                   (eq? 'eval:lookup (caar exprs))
                   (hash-ref (code-state-forwards state) (cadar exprs))))
            (define forward (and forward-e+stx (cdr forward-e+stx)))
            (unless forward
              (eval (clean (car exprs)) ns))
            (if (and (pair? (car exprs))
                     (eq? 'code:comment  (caar exprs)))
                (loop (cdr exprs) (cdr stxes) (cdr picts)
                      (if (null? (code-state-code state))
                          (struct-copy code-state state
                                       [code-desc (cons (cadar exprs)
                                                        (code-state-code-desc state))]
                                       [all-code (if (code-state-keep-code? state)
                                                     (cons (car stxes) (code-state-all-code state))
                                                     (code-state-all-code state))])
                          (struct-copy code-state state
                                       [post-code-desc (cons (cadar exprs)
                                                             (code-state-post-code-desc state))]
                                       [all-code (if (code-state-keep-code? state)
                                                     (cons (car stxes) (code-state-all-code state))
                                                     (code-state-all-code state))])))
                (loop (cdr exprs) (cdr stxes) (cdr picts)
                      (struct-copy code-state state
                                   [code (cons (or forward (at-scale state (car picts)))
                                               (code-state-code state))]
                                   [accum-code-height (+ (if (code-state-keep-code? state)
                                                             (pict-height (or forward (car picts)))
                                                             0)
                                                         (code-state-accum-code-height state))]
                                   [all-code (if (code-state-keep-code? state)
                                                 (cons (if forward
                                                           (car forward-e+stx)
                                                           (car stxes))
                                                       (code-state-all-code state))
                                                 (code-state-all-code state))])))]
           [(#:example #:example/left)
            (case (cadr exprs)
              [(#:!)
               (define new-state 
                 (struct-copy code-state state
                              [accum (cons (at-scale state (car picts))
                                           (code-state-accum state))]
                              [all-code (cons `(module+ test ,(car stxes))
                                              (code-state-all-code state))]))
               (emit new-state)
               (set! tests (cons (lambda ()
                                   (eval (clean (car exprs)) ns))
                                 tests))
               (loop (cddr exprs) (cddr stxes) (cddr picts) (emitted new-state))]
              [(#:=> #:=>+ #:=>!)
               (define eval? (not (eq? (cadr exprs) '#:=>!)))
               (define new-state (struct-copy code-state state
                                              [accum (cons (at-scale state
                                                                     (example-result (car picts)
                                                                                     (caddr picts)))
                                                           (code-state-accum state))]
                                              [all-code
                                               (if eval?
                                                   (cons `(module+ test (check-equal? ,(car stxes) ,(caddr stxes)))
                                                         (code-state-all-code state))
                                                   (code-state-all-code state))]))
               (emit new-state)
               (when eval?
                 (add-test! (car exprs) (caddr exprs)))
               (cond
                [(eq? (cadr exprs) '#:=>+)
                 (define newer-state 
                   (struct-copy code-state new-state
                                [start-here? #f]
                                [accum (cons (at-scale state (cadddr picts))
                                             (code-state-accum new-state))]))
                 (emit newer-state)
                 (loop (cddddr exprs) (cddddr stxes) (cddddr picts) (emitted newer-state))]
                [else
                 (loop (cdddr exprs) (cdddr stxes) (cdddr picts) (emitted new-state))])]
              [(#:+)
               (eval (clean (car exprs)) ns)
               (loop (cddr exprs) (cddr stxes) (cddr picts)
                     (struct-copy code-state state
                                  [accum (cons (at-scale state (car picts))
                                               (code-state-accum state))]
                                  [all-code
                                   (cons `(module+ test ,(car stxes))
                                         (code-state-all-code state))]))]
              [(#:fail)
               (define new-state
                 (struct-copy code-state state
                              [accum (cons (at-scale state
                                                     (example-result (car picts)
                                                                     (caddr picts)
                                                                     #:fail (caddr exprs)))
                                           (code-state-accum state))]
                              [all-code
                               (cons `(module+ test (check-error ,(car stxes) ,(caddr stxes)))
                                     (code-state-all-code state))]))
               (emit new-state)
               (loop (cdddr exprs) (cdddr stxes) (cdddr picts) (emitted new-state))]
              [else
               (error "bad in example mode" (cadr picts))])]
           [(#:example-def)
            (eval (clean (car exprs)) ns)
            (loop (cdr exprs) (cdr stxes) (cdr picts)
                  (struct-copy code-state state
                               [example-def (cons (at-scale state (car picts))
                                                  (code-state-example-def state))]
                               [all-code
                                (cons `(module+ test ,(car stxes))
                                      (code-state-all-code state))]))]
           [else
            (error "bad mode" (code-state-mode state))])])]))
  ;; Run tests, not that all definitions are in place
  (for ([t (in-list (reverse tests))])
    (t)))
  
(define (clean s)
  (cond
   [(pair? s)
    (case (car s)
      [(unsyntax) (void)]
      [(code:comment) (void)]
      [else
       (cons (clean (car s)) (clean (cdr s)))])]
   [(eq? s 'code:blank)
    (void)]
   [(eq? s 'QUOTE) 'quote]
   [(eq? s 'QQUOTE) 'quote]
   [else 
    s]))

(define (emit given-state #:force? [force? #f] #:done-code? [done-code? #f])
  (define should? (should-emit? given-state))
  (when (or force?
            should?
            (and done-code?
                 use-transitions?
                 (code-state-keep-code? given-state)
                 (pair? (code-state-code given-state))))
    (define state (or (if (or force? should?) given-state (code-state-last-emitted given-state))
                      given-state))
    (define ct (reverse (code-state-code-desc state)))
    (define c (reverse (code-state-code state)))
    (define pct (reverse (code-state-post-code-desc state)))
    (define d (reverse (code-state-example-def state)))
    (define a (reverse (code-state-accum state)))
    (define ct/p (format-comment ct))
    (define (slide-content jump-i)
      (vc-append*
       gap-size
       ct/p
       (if (null? c)
           'nothing
           (let ([p (code-background
                     (para (scale (for/fold ([p (apply vl-append (current-line-sep) c)]) ([b (in-list
                                                                                              (if (zero? jump-i)
                                                                                                  (reverse
                                                                                                   (code-state-code-balloons state))
                                                                                                  null))])
                                    (add-code-balloon p b #:scale (code-state-scale state)))
                                  0.8))
                     ct)])
             (cond 
              [(zero? jump-i) p]
              [else
               (define ai (fast-start jump-i))
               (refocus (pin-over (ghost p)
                                  (* (/ (+ (- (pict-width p) client-w) gap-size) 2) ai)
                                  (* -1 (+ (if (pict? ct/p) (+ gap-size (pict-height ct/p)) 0) (* 3 gap-size)) ai)
                                  (scale (launder p) (- 1 (* 0.97 (/ (- (expt (- (* 1.5 ai) 0.5) 2.0) 0.25) 0.75)))))
                        p)])))
       (format-comment pct)
       (let ([dp (format-code-lines d)]
             [ap (format-code-lines a)])
         (cond
          [(eq? dp 'nothing) ap]
          [(eq? ap 'nothing) dp]
          [(eq? (code-state-mode state) '#:example/left)
           (define together (vl-append gap-size dp ap))
           (let ([p (blank (current-para-width)
                           (pict-height together))])
             (refocus (cc-superimpose p together)
                      p))]
          [else
           (vc-append gap-size
                      dp
                      (let ([p (blank (current-para-width)
                                      (pict-height ap))])
                        (refocus (cc-superimpose p ap)
                                 p)))]))))
    (parameterize ([current-slide-assembler (make-state-slide-assembler state)])
      (when should?
        (slide #:title (code-state-title state)
               #:layout 'top
               (slide-content 0)))
      (when (and done-code? (code-state-keep-code? given-state) (pair? c) use-transitions?)
        (play-n
         #:title (code-state-title state)
         #:layout 'top
         #:skip-first? #t
         #:skip-last? #t
         slide-content)))
    (when (code-state-start-here? state)
      (start-at-recent-slide))))

(define (vc-append* sep . l)
  (apply vc-append sep (for/list ([i (in-list l)]
                                  #:unless (eq? i 'nothing))
                         i)))

(define (make-state-slide-assembler state)
  (let ([a (current-slide-assembler)])
    (lambda (title i p)
      (define r (a title i p))
      (ct-superimpose
       (lt-superimpose
        (blank client-w client-h)
        (let ([bg (let ([f (/ (code-state-done-height state)
                              (code-state-total-height state))])
                    (frame (ct-superimpose
                            (colorize (filled-rectangle 64 80) "white")
                            (frame (colorize (filled-rectangle 64 (* 80 f))
                                             code-background-color)))
                           #:line-width 2))])
          (define draw-bg (make-pict-drawer bg))
          (define p (make-object dc-path%))
          (send p move-to -2 -2)
          (send p line-to -2 (+ 2 (pict-height bg)))
          (send p line-to (+ 2 (pict-width bg)) (+ 2 (pict-height bg)))
          (send p line-to (+ 2 (pict-width bg)) 21)
          (send p line-to (- (pict-width bg) 22) -2)
          (send p close)
          (define tri (make-object dc-path%))
          (send tri move-to 44 0)
          (send tri line-to 44 20)
          (send tri line-to 64 20)
          (send tri close)
          (dc (lambda (dc dx dy)
                (define r (make-object region% dc))
                (define c (send dc get-clipping-region))
                (send r set-path p dx dy)
                (when c
                  (send r intersect c))
                
                (send dc set-clipping-region r)
                (draw-bg dc dx dy)
                
                (send dc set-clipping-region c)
                
                (define br (send dc get-brush))
                (define pn (send dc get-pen))
                
                (send dc set-pen "black" 1 'solid)
                (send dc set-brush "white" 'solid)
                (send dc draw-path tri dx dy)
                
                (send dc set-pen "black" 2 'solid)
                (send dc draw-line (+ dx 44) dy (+ dx 64) (+ dy 20))
                
                (send dc set-pen pn)
                (send dc set-brush br))
              (pict-width bg)
              (pict-height bg))))
       (let ([bg (blank client-w client-h)])
         (refocus (if ((pict-width r) . <= . client-w)
                      (ct-superimpose r bg)
                      (lt-superimpose r bg))
                  bg))))))
      
(define (emitted state #:force? [force? #f] #:done-code? [done-code? #f])
  ((if done-code? done-code values)
   (cond
    [(or force? (should-emit? state))
     (struct-copy code-state state
                  [accum null]
                  [start-here? #f]
                  [scale 1]
                  [code-balloons null]
                  [last-emitted state])]
    [else state])))

(define (done-code state)
  (struct-copy code-state state
               [code-desc null]
               [code null]
               [post-code-desc null]
               [example-def null]
               [code-balloons null]
               [accum-code-height 0]
               [done-height (+ (code-state-done-height state)
                               (code-state-accum-code-height state))]))

(define (at-scale state p)
  (define s (code-state-scale state))
  (if (= s 1)
      p
      (scale p s)))

(define (should-emit? state)
  (define ct (code-state-code-desc state))
  (define c (code-state-code state))
  (define pct (code-state-post-code-desc state))
  (define d (code-state-example-def state))
  (define a (code-state-accum state))
  (not (and (or (not (eq? '#:code (code-state-mode state)))
                (and (null? ct)
                     (null? c)
                     (null? pct)))
            (null? a))))

(define (format-code-lines l)
  (if (null? l)
      'nothing
      (para (apply vl-append (current-line-sep) l))))

(define rx:bullet #rx"^ *[*] +")

(define (format-comment ct)
  (cond
   [(null? ct) 'nothing]
   [(null? (cdr ct))
    (apply para
           #:width (* client-w 0.9)
           (format-comment-content (car ct)))]
   [(for/and ([x (in-list (cdr ct))])
      (regexp-match? rx:bullet x))
    (htl-append gap-size
                (apply para #:fill? #f (format-comment-content (car ct)))
                (apply
                 vl-append
                 (current-line-sep)
                 (for/list ([s (in-list (cdr ct))])
                   (apply item
                          #:width (* client-w 1/2)
                          (format-comment-content (regexp-replace rx:bullet s ""))))))]
   [else
    (inset
     (apply vc-append
            gap-size
            (format-comment (list (car ct)))
            (for/list ([s (in-list (cdr ct))])
              (apply para
                     #:width (* client-w 0.9)
                     #:align 'right
                     (format-comment-content s))))
     0 0 0 (* -1 gap-size))]))

(define (format-comment-content s)
  (let loop ([s s])
    (define (recur start [end (string-length s)])
      (if (= start end)
          null
          (loop (regexp-replace #rx" $" (substring s start end) ""))))
    (cond
     [(regexp-match-positions #rx"`[^`]*`" s)
      => (lambda (m)
           (append
            (recur 0 (caar m))
            (list (typeset-code (read-syntax
                                 #f
                                 (open-input-string (substring s (add1 (caar m)) (sub1 (cdar m)))))))
            (recur (cdar m))))]
     [(regexp-match-positions #rx"^Helper: " s)
      => (lambda (m)
           (cons (it "Helper:") (loop (substring s (cdar m)))))]
     [(regexp-match-positions #rx"/[^/]*/" s)
      => (lambda (m)
           (append
            (recur 0 (caar m))
            (list (it (substring s (add1 (caar m)) (sub1 (cdar m)))))
            (recur (cdar m))))]
     [(regexp-match-positions #rx"\"[^\"]*\"" s)
      => (lambda (m)
           (append
            (recur 0 (caar m))
            (list (t (string-append "“" (substring s (add1 (caar m)) (sub1 (cdar m))) "”")))
            (recur (cdar m))))]
     [else (list s)])))

(define code-background-color (tweak "beige"))

(define (code-background p ct)
  (inset (cc-superimpose
          (colorize (filled-rectangle (+ (pict-width p) gap-size)
                                      (+ (pict-height p) gap-size))
                    code-background-color)
          p)
         gap-size (if (null? ct) 0 gap-size) gap-size gap-size))

(define (example-result ex r #:fail [fail #f])
  (vl-append
   (current-line-sep)
   ex
   (htl-append (tt "⇒ ")
               (if fail
                   (colorize (it (string-append "error: " fail)) "red")
                   r))))

(define scope-example-color (tweak "lightgray"))
(define scope-example-alt-color (tweak "lightskyblue"))
(define scope-note-color (tweak "burlywood"))

(define (scope-example p
                       #:square? [square? #t]
                       #:wide? [wide? #f]
                       #:padding [padding 0]
                       #:end-margin [end-margin gap-size]
                       #:alt-color? [alt-color? #f])
  (inset (refocus (cc-superimpose
                   (cloud (+ (pict-width p) (* 2 gap-size) (* 2 padding))
                          (+ (pict-height p) gap-size (* 2 padding))
                          #:style (cond
                                   [square? '(square wide)]
                                   [wide? '(wide)]
                                   [else null])
                          (if alt-color?
                              scope-example-alt-color
                              scope-example-color))
                   p)
                  p)
         0 0 0 end-margin))

(define (add-code-balloon code spec #:scale s)
  (cond
   [(not spec) code]
   [else
    (define cell (tt " "))
    (define line (car spec))
    (define col (cadr spec))
    (define content (caddr spec))
    (pin-balloon content
                 code
                 (* s col (pict-width cell))
                 (* s (- line 0.5) (+ (current-code-line-sep) (pict-height cell))))]))

(balloon-enable-3d #f)

(define (scope-balloon p
                       #:code? [code? #t]
                       #:alt-color? [alt-color? #f]
                       #:square? [square? #f]
                       #:spike [spike 'sw]
                       #:spike-y* [spike-y* 1]
                       #:spike-x* [spike-x* 1])
  (define (wrap p) (scope-example p #:padding gap-size #:square? square? #:end-margin 0 #:alt-color? alt-color?))
  (wrap-balloon (if code?
                    (inset (let ([q (wrap p)])
                             (refocus (cc-superimpose (cellophane (wrap (ghost (inset p (/ gap-size 2)))) 0.5)
                                                      q)
                                      q))
                           (- gap-size))
                    (inset p (/ gap-size 2)))
                spike
                (* spike-x*
                   (case spike
                     [(e ne se) (* 4 gap-size)]
                     [(s n) 0]
                     [else (* -4 gap-size)]))
                (* spike-y* (case spike
                              [(w) 0]
                              [(nw ne) (* -2 gap-size)]
                              [(n) (* -4 gap-size)]
                              [(s) (* 4 gap-size)]
                              [else (* 2 gap-size)]))
                (if code? (if alt-color? scope-example-alt-color scope-example-color) scope-note-color)
                (if code? 32 16)))

;; ----------------------------------------

(define (count-code-height exprs picts)
  (let loop ([exprs exprs] [picts picts] [state #f])
    (cond
     [(null? exprs) 0]
     [else
      (case (car exprs)
        [(#:part #:external #:external/code #:eval
                 #:code-balloon #:code-balloon+ #:code-balloon/- #:code-balloon/-- #:code-balloon/+-
                 #:section #:scale)
         (loop (cddr exprs) (cddr picts) state)]
        [(#:eval-example)
         (loop (cdddr exprs) (cdddr picts) state)]
        [(#:eval/forward)
         (+ (pict-height (caddr picts))
            (loop (cdddr exprs) (cdddr picts) state))]
        [(#:code)
         (loop (cdr exprs) (cdr picts) '#:code)]
        [(#:temporary-code)
         (loop (cdr exprs) (cdr picts) ' #:temporary-code)]
        [(#:example #:example/left)
         (loop (cdr exprs) (cdr picts) '#:example)]
        [(#:example-def)
         (loop (cdr exprs) (cdr picts) '#:example-def)]
        [(#:start-here #:next)
         (loop (cdr exprs) (cdr picts) state)]
        [else
         (when (keyword? (car exprs)) (error "unrecognized keyword" (car exprs)))
         (case state
           [(#:code)
            (cond
             [(and (pair? (car exprs))
                   (eq? 'eval:lookup (caar exprs)))
              (loop (cdr exprs) (cdr picts) state)]
             [(and (pair? (car exprs))
                   (eq? 'code:comment  (caar exprs)))
              (loop (cdr exprs) (cdr picts) state)]
             [else
              (+ (loop (cdr exprs) (cdr picts) state)
                 (pict-height (car picts)))])]
           [(#:temporary-code)
            (loop (cdr exprs) (cdr picts) state)]
           [(#:example)
            (case (cadr exprs)
              [(#:!)
               (loop (cddr exprs) (cddr picts) state)]
              [(#:=> #:=>+ #:=>!)
               (cond
                [(eq? (cadr exprs) '#:=>+)
                 (loop (cddddr exprs) (cddddr picts) state)]
                [else
                 (loop (cdddr exprs) (cdddr picts) state)])]
              [(#:+)
               (loop (cddr exprs) (cddr picts) state)]
              [(#:fail)
               (loop (cdddr exprs) (cdddr picts) state)]
              [else
               (error "bad in example mode" (cadr picts))])]
           [(#:example-def)
            (loop (cdr exprs) (cdr picts) state)]
           [else
            (error "bad mode" state)])])])))

;; ----------------------------------------
  
(struct printable-comment (c)
        #:property prop:custom-write
        (lambda (v o mode)
          (fprintf o ";; ~a~a" (printable-comment-c v) (make-string 80 #\space))))

(struct printable-blank ()
        #:property prop:custom-write
        (lambda (v o mode)
          (newline o)))

(define (printable s)
  (cond
   [(pair? s)
    (case (car s)
      [(unsyntax) (void)]
      [(code:comment) (printable-comment (cadr s))]
      [else
       (cons (printable (car s)) (printable (cdr s)))])]
   [(eq? s 'code:blank)
    (printable-blank)]
   [(eq? s 'QUOTE) 'quote]
   [(eq? s 'QQUOTE) 'quote]
   [else s]))

(define (print-form stx o indent-col start-col col)
  (cond
   [(pair? stx)
    ;; Must be a test case
    (define s (format "(~a " (car stx)))
    (display s o)
    (define test-col (+ indent-col (string-length s)))
    (print-form (cadr stx) o test-col (syntax-column (cadr stx)) 0)
    (newline o)
    (display (make-string test-col #\space) o)
    (define new-col (print-form (caddr stx) o test-col (syntax-column (caddr stx)) 0))
    (fprintf o ")")
    (add1 new-col)]
   [(skip? stx) col]
   [else
    (define s (syntax-e stx))
    (cond
     [(eq? s 'code:blank)
      (cond
       [(= col start-col)
        (newline o)
        col]
       [else
        (fprintf o " ")
        (add1 col)])]
     [(eq? s 'QUOTE) (print-form (datum->syntax #f 'quote stx) o indent-col start-col col)]
     [(eq? s 'QQUOTE) (print-form (datum->syntax #f 'quote stx) o indent-col start-col col)]
     [(not (pair? s))
      (define str (format "~s" s))
      (display str o)
      (+ col (string-length str))]
     [(and (pair? s)
           (eq? 'code:comment (syntax-e (car s))))
      (fprintf o ";; ~a" (syntax-e (cadr s)))
      (add1 col)]
     [(and (pair? s)
           (memq (syntax-e (car s))
                 '(quote quasiquote unquote))
           (equal? 1 (syntax-span (car s)))
           (null? (cddr s)))
      (fprintf o (case (syntax-e (car s))
                   [(quote) "'"]
                   [(unquote) ","]
                   [(unquote-splicing) ",@"]
                   [(quasiquote) "`"]))
      (print-form (cadr s) o indent-col start-col (add1 col))]
     [(and (pair? s)
           (memq (syntax-e (car s))
                 '(unquote-splicing))
           (equal? 2 (syntax-span (car s)))
           (null? (cddr s)))
      (fprintf o (case (syntax-e (car s))
                   [(unquote-splicing) ",@"]))
      (print-form (cadr s) o indent-col start-col (+ 2 col))]
     [else
      (unless (list? s) (error "can only print lists"))
      (fprintf o "~a" (or (syntax-property stx 'paren-shape) #\())
      (define end-col
        (let loop ([first? #t] [line (syntax-line stx)] [elems s] [col col])
          (cond
           [(null? elems) col]
           [(and (syntax-line (car elems))
                 (not (= line (syntax-line (car elems)))))
            (newline o)
            (define new-col (max 0 (- (syntax-column (car elems)) start-col)))
            (display (make-string (+ indent-col new-col) #\space) o)
            (loop #f
                  (syntax-line (car elems))
                  (cdr elems)
                  (print-form (car elems) o indent-col start-col new-col))]
           [else
            (unless first? (display " " o))
            (define new-col 
              (print-form (car elems) o indent-col start-col (if first? col (add1 col))))
            (loop #f
                  line
                  (cdr elems)
                  new-col)])))
      (fprintf o "~a" (case (syntax-property stx 'paren-shape)
                        [(#\[) "]"]
                        [(#\{) "}"]
                        [else ")"]))
      (add1 end-col)])]))

(define (skip? stx)
  (define s (and (syntax? stx) (syntax-e stx)))
  (and (pair? s)
       (eq? 'unsyntax (syntax-e (car s)))))

(define (stx-column s)
  (or (and (syntax? s)
           (syntax-column s))
      0))

(define (print-code stx o col in-test? keep-tests?)
  (cond
   [(and (pair? stx) (eq? 'module+ (car stx)))
    (cond
     [(not keep-tests?)
      (values col #f)]
     [else
      (define inner-stx (caddr stx))
      (cond
       [(skip? inner-stx)
        (values col in-test?)]
       [(and (not in-test?)
             (syntax? inner-stx)
             (eq? 'code:blank (syntax-e inner-stx)))
        ;; avoid a blank first line in `(module+ test ...)`
        (values col #f)]
       [in-test?
        (unless (zero? col)
          (newline o)
          (fprintf o "  "))
        (values (print-form inner-stx o 2 (stx-column inner-stx) 0)
                #t)]
       [else
        (fprintf o "\n\n(module+ test\n")
        (fprintf o "  ")
        (print-code stx o 0 #t keep-tests?)])])]
   [(and (syntax? stx)
         (eq? 'code:blank (syntax-e stx)))
    (cond
     [in-test?
      (values col #t)]
     [else
      (unless (zero? col)
        (newline o)
        (newline o))
      (values 0 #f)])]
   [in-test?
    (fprintf o ")\n\n")
    (print-code stx o 0 #f keep-tests?)]
   [else
    (unless (zero? col)
      (newline o))
    (values (print-form stx o 0 (stx-column stx) 0)
            #f)]))

(define (filter-dup-defn l)
  ;; In one case, it's easiest to show a definition twice, so filter
  ;; the first one; the duplicate must appear right after the original,
  ;; possibly separated by a blank
  (let loop ([l l])
    (cond
     [(null? l) null]
     [(or (and (pair? (cdr l))
               (syntax? (car l))
               (syntax? (cadr l))
               (equal? (syntax->datum (car l)) (syntax->datum (cadr l))))
          (and (pair? (cdr l))
               (pair? (cddr l))
               (syntax? (car l))
               (syntax? (cadr l))
               (syntax? (caddr l))
               (eq? 'code:blank (syntax-e (cadr l)))
               (equal? (syntax->datum (car l)) (syntax->datum (caddr l)))))
      (loop (cddr l))]
     [else (cons (car l) (loop (cdr l)))])))

(define (gather-all-code l t-l #:keep-tests? [keep-tests? #t])
  (define o (open-output-bytes))
  (fprintf o "#lang racket\n")
  (fprintf o "(require racket/set)")
  (let ([t-l (append (list
                      `(module+ test ,#'(require rackunit))
                      `(module+ text
                        ,#'(define-syntax-rule (check-error e rx)
                             (check-exn (lambda (v)
                                          (and (exn:fail? v)
                                               (regexp-match? rx (exn-message v))))
                                        (lambda () e)))))
                     t-l)])
    (let ([l (if keep-tests?
                 (append t-l l)
                 l)])
      (define-values (end-col end-in-test?)
        (for/fold ([col 1] [in-test? #f]) ([stx (in-list (filter-dup-defn l))])
          (print-code stx o col in-test? keep-tests?)))
      (unless (zero? end-col)
        (newline o))))
  (get-output-string o))
