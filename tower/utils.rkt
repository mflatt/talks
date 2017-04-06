#lang slideshow
(require slideshow/code
         slideshow/balloon
         (only-in mzlib/etc begin-with-definitions)
         (only-in texpict/mrpict record)
         texpict/symbol)

(provide (all-defined-out)
         begin-with-definitions
         record
         (all-from-out texpict/symbol))

(define WIDE (* 0.9 client-w))

(define (bright p) (colorize p "blue"))

(current-keyword-list
 (append '("#%parens"
           "class"
           "define-place"
           "define-thing"
           "define-verbs"
           "define-one-verb"
           "define-everywhere"
           "define/public"
           "define/contract"
           "define-three"
           "->"
           "="
           "for-syntax"
           "identifier-syntax"
           "#%app"
           "define-package"
           "open-package"
           "syntax-parse")
         (current-keyword-list)))

(define (ptt s) (colorize (tt s) (current-base-color)))
(define (pstr s) (colorize (tt s) (current-literal-color)))

(define longish (- client-w (* 2 gap-size)))

;; ----------------------------------------

(define (demo-slide #:hack? [hack? #f] desc)
  (slide
   #:name (if hack? "<programming>" "<demo>")
   (hbl-append (bt "[insert ") 
               (if hack? (bt "programming") (bt "demo"))
               (bt " here]"))
   (parameterize ([current-main-font `(italic . ,(current-main-font))])
     (para #:align 'center desc))))

;; ----------------------------------------

(define-syntax-rule (slide/gauge #:level lvl . rest)
  (slide . rest))

(define (call-with-gauge lvl thunk)
  (thunk))

(define (gauge x y z w) (void))

(define gauge:none (gauge 0 0 0 'l))
(define gauge:lisp0 (gauge 0.20 0 0 'l))
(define gauge:basic-lisp (gauge 0.5 0 0 'l))
(define gauge:taste-of-plt0 (gauge 0.5 0.0 0.05 'l))
(define gauge:taste-of-scheme (gauge 0.5 0.10 0.10 'l))
(define gauge:taste-of-plt (gauge 0.5 0.10 0.15 'l))
(define gauge:more-lisp (gauge 0.60 0.10 0.15 'l))
(define gauge:and-more-lisp (gauge 0.75 0.10 0.15 'l))
(define gauge:all-lisp (gauge 1.0 0.10 0.15 'l))

(define (gauge:back-to-scheme rel)
  (gauge 1.0 (+ 0.10 (* rel 0.80)) 0.15 's))
(define gauge:all-scheme (gauge 1.0 1.0 0.15 's))

(define (gauge:partial-plt-scheme v)
  (gauge 1.0 1.0 (+ 0.25 (* 0.75 v)) 'p))
(define gauge:plt-scheme (gauge:partial-plt-scheme 1.0))

(define gauge:just-lisp (gauge 1.0 0 0 'l))
(define gauge:lisp+scheme (gauge 1.0 1.0 0 's))

;; ----------------------------------------

(define (rec-tt s) (inset (scale (tt s) 0.8) 5))

(define (rec-sub r) (inset r 5))

(define (round-wrap #:color [col "blue"] p)
  (let ([p (inset
            p
            gap-size)])
    (cc-superimpose
     (cellophane (colorize (filled-rounded-rectangle (pict-width p) (pict-height p) gap-size) col)
                 0.25)
     p)))

(define (parse-stage p) (round-wrap p #:color "lightblue"))
(define s-expression-stage
  (parse-stage
   (vc-append
    (current-line-sep)
    (code (...
           (....)
           ...))
    (t "S-expression"))))
(define ast-stage
  (parse-stage
   (vc-append
    (current-line-sep)
    (let ([p (record (rec-tt "function")
                     (rec-tt "args")
                     (rec-tt "body"))])
      (cc-superimpose
       (colorize (filled-rectangle (pict-width p) (pict-height p)) "white")
       p))
    (t "AST"))))

(define (as-file* p)
  (as-file #f (inset p 5 10)))
    
(define (as-file s content)
  (let ([w (max (* 3 gap-size)
                (+ 6 (pict-width content)))]
        [h (max (* 4 gap-size)
                (+ gap-size (pict-height content)))])
    (let ([icon (file-icon w h "beige")])
      (let ([file (cc-superimpose
                   icon
                   content)])
        (if (not s)
            file
            (inset
             (vc-append
              (current-line-sep)
              file
              s)
             0 0 (/ (min 0 (- (pict-width icon) (pict-width s))) 2) 0))))))

(define trans-arrow (colorize (arrow (* 2 gap-size) 0) "forestgreen"))   

(define (pipeline left-adj left-arrow-decorate mid-adj right-arrow-decorate right-adj)
  (hc-append
   (* 2 gap-size)
   (left-adj (parse-stage (as-file (t "Source") (blank))))
   (left-adj (left-arrow-decorate trans-arrow))
   (mid-adj s-expression-stage)
   (right-adj (right-arrow-decorate trans-arrow))
   (right-adj ast-stage)))

(define fade (lambda (p) (cellophane p 0.25)))

(define expands (colorize sym:implies "blue"))

;; ----------------------------------------

(define (ltlc-find a b)
  (let-values ([(xt yt) (lt-find a b)]
               [(x y) (ltl-find a b)])
    (values x (/ (+ yt y) 2))))

(define (syntax-object-record val pos context-extra)
  (let ([datum (rec-tt "datum")]
        [srcloc (rec-tt "srcloc")]
        [context (rec-tt "context")]
        [file (tt (format "file.ss:1:~a" pos))]
        [env (htl-append (tt "{ ")
                         (vl-append
                          (current-code-line-sep)
                          (hbl-append
                           (tt "mark ")
                           (text "3" `(superscript . ,(current-main-font)) (current-font-size))
                           (tt ","))
                          (if context-extra
                              (hbl-append context-extra (tt ","))
                              (inset (blank) 0 (- (current-code-line-sep)) 0 0))
                          (hbl-append
                           (tt "rename ")
                           (code tmp^2)
                           (tt " to ")
                           (code tmp_1)
                           (tt ","))
                          (hbl-append
                           (code lambda)
                           (tt " = ")
                           (colorize (tt "lambda @ kernel") (current-literal-color))
                           (tt ","))
                          (tt "... }")))]
        [add-arrows (lambda (p srcs dests)
                      (for/fold ([p p]) ([src srcs]
                                         [dest dests])
                        (pin-arrow-line
                         (/ gap-size 2) p
                         src rc-find
                         dest ltlc-find
                         #:line-width 2
                         #:color "orange")))])
    (add-arrows
     (inset
      (ht-append
       (* 3 gap-size)
       (record
        (rec-tt "syntax-object")
        datum
        srcloc
        context)
       (vl-append
        (* 2 gap-size)
        (blank)
        val
        file
        env))
      gap-size 0 0 0)
     (list datum srcloc context)
     (list val file env))))

;; ----------------------------------------

(define (demo p dir show #:find [find #f] . what)
  (refocus
   (lt-superimpose
    (show
     (pin-balloon (wrap-balloon
                   (scale
                    (apply vl-append (current-line-sep)
                           (hbl-append (bt "Demo") (t ":"))
                           (let loop ([what what])
                             (cond
                               [(null? what) null]
                               [(null? (cdr what)) 
                                (list (hbl-append (t " ") (car what)))]
                               [else
                                (cons (hbl-append (t " ") (car what) (t ","))
                                      (loop (cdr what)))])))
                    0.75)
                   dir
                   (case dir
                     [(sw nw) (- (/ gap-size 2))]
                     [(se ne) (/ gap-size 2)])
                   (case dir
                     [(sw se) (/ gap-size 2)]
                     [(nw ne) (- (/ gap-size 2))])
                   balloon-color 16)
                  (ghost p) p 
                  (or find
                      (lambda (a b)
                        (let-values ([(x y) (rt-find a b)])
                          (values (- x gap-size) y))))))
    p)
   p))

;; ----------------------------------------

#;
(define outline
  (make-outline
   'examples "Examples" #f
   (* 2 gap-size)
   'macros "Lisp-Style Macros" (lambda (s)
                                 (vc-append
                                  (- (/ gap-size 2))
                                  (it "Scheme")
                                  (let ([s (t " ")])
                                    (refocus (hbl-append (* 3 gap-size) (it "Lisp") s (it "Racket"))
                                             s))
                                  (scale gauge:plt-scheme 3.0)))
   'lex "Scheme Macros and Lexical Scope" (lambda (s)
                                            (scale (pipeline fade values values values fade)
                                                   0.75))
   'plt "Racket Macros" (lambda (s)
                          (scale (pipeline fade values fade fade values)
                                 0.75))
   (* 2 gap-size)
   'refs "References and Related Work" #f))
