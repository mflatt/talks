#lang at-exp slideshow
(require slideshow/code
         slideshow/balloon
         slideshow/face
         slideshow/flash
         (only-in texpict/mrpict record)
         scheme/gui/base
         "utils.rkt"
         "lisp.rkt"
         "scheme.rkt"
         "plt.rkt")

;; ----------------------------------------

(define plt-logo (bitmap (collection-file-path "PLT-206.png" "icons")))

(define (author who where)
  (vc-append
   (current-line-sep)
   (colorize (bt who) "blue")
   (blank (/ (current-font-size) 3))
   (scale/improve-new-text
    (t where)
    0.8)))

(slide
 (titlet "Languages in Racket")
 #;
 (vc-append
  (current-line-sep)
  (titlet "Turning a Tower of Babel")
  (titlet "into a Beautiful Racket"))

 (blank)
 plt-logo
 (blank)
 
 (author "Matthew Flatt" "University of Utah"))

;; ----------------------------------------

(define (why-slide java c# racket)
  (slide
   #:title "Why Language Extensibility?"
   (colorize (bt "Your programming language isn't good enough, yet")
             "blue")
   (cc-superimpose
    (java
     (vc-append
      (* 2 (current-line-sep))
      (scale (bitmap "jls.jpg") 0.8)
      (t "684 pages")))
    (ht-append
     (* 3 gap-size)
     (c#
      (vc-append
       (* gap-size 1/2)
       (para #:fill? #f (bt "C#") "(De Smet)")
       (colorize (it "Democratizing reactive programming") "red")
       (colorize (it "Democratizing data access") "red")
       (colorize (t "...") "red")))
     (racket
      (vc-append
       (* gap-size 1/2)
       (bt "Racket")
       (colorize (it "Democratizing language design") "forestgreen")))))))

(if condense?
    (skip-slides 1)
    (why-slide ghost ghost ghost))
(why-slide values ghost ghost)
#|
(if condense?
    (skip-slides 1)
    (why-slide ghost values ghost))
(why-slide ghost values values)
|#

;; ----------------------------------------

(demo-slide "example languages")

;; ----------------------------------------

(define ex-scale 0.6)

(slide
 (bright (para #:width (+ (current-para-width) gap-size) "Different levels of language extension..."))
 (blank)
 (item "Syntactic abstraction")
 (scale
  (code (define (roman->number str)
          (rx-case str
           ["([XV]*)(I{1,4})" (xv i) (+ (roman->number xv)
                                        (string-length i))]
           ....)))
  ex-scale)
 (blank)
 (item "New language constructs")
 (scale (code (class object% (define/public method ....) ....))
        ex-scale)
 (blank)
 (item "New languages")
 (vl-append
  gap-size
  (scale (code (: factorial (Number -> Number)))
         ex-scale)
  (scale (hbl-append (tt "@") (code section) (ptt "{") (pstr "Hello") (ptt "}"))
         ex-scale)
  (scale (hbl-append (code int) (tt " ") (code f) (code (int n)) (ptt " { ") (code return n) (code +) (code 1) (ptt "; }"))
         ex-scale))
 (bright (para #:align 'right "... in one framework" )))

;; ----------------------------------------

(define motivation-title "Implementing a Text Adventure Game")

(slide
 #:title motivation-title
 (apply
  vl-append
  (map
   tt
   '("You're standing in a field."
     "There is a house to the north."
     "> north"
     "You are standing in front of a house."
     "There is a door here."
     "> open door"
     "The door is locked."
     "> "))))

(demo-slide "play the game")

(define (et s) (inset (colorize (bt s) "blue") 0 0 (/ gap-size 2) 0))

(define places-item  (item #:fill? #f (et "Places")))
(define things-item (item #:fill? #f (et "Things")))
(define verbs-item (item #:fill? #f (et "Verbs")))

(slide
 #:title motivation-title
 'alts
 (let* ([p (vl-append
            gap-size
            places-item
            things-item
            (vl-append
             (* 2 (current-line-sep))
             verbs-item
             (subitem "global intransitive verbs")
             (subitem "place-local intransitive verbs")
             (subitem "thing-specific transitive verbs")))]
        [plain-p p]
        [obj?-label (t "Objects?")]
        [objects (inset obj?-label (* 1/2 gap-size))]
        [meth?-label (t "Methods?")]
        [methods (inset meth?-label (* 1/2 gap-size))]
        [objection (lambda (content)
                     (let ([q (inset
                               (para #:width (* client-w 0.4)
                                     #:fill? #f
                                     content)
                               (/ gap-size 2))])
                       (frame
                        (cc-superimpose
                         (colorize (filled-rectangle (pict-width q) (pict-height q)) "pink")
                         q))))]
        [obj-obj (objection "Need not only serialize, but save & restore variables")]
        [meth-obj (objection "Must convert between string command and method call")]
        [b1 (wrap-balloon objects 'w (* gap-size -3) 0)]
        [b2 (let-values ([(x1 y1) (rc-find p places-item)]
                         [(x2 y2) (rc-find p things-item)])
              (wrap-balloon objects 'w 
                            (+ (* gap-size -3) (- x2 x1))
                            (- y2 y1)))]
        [p (pin-balloon b2 p things-item rc-find)]
        [p (pin-balloon b1 p places-item rc-find)]
        [obj?-p p]
        [b3 (wrap-balloon methods 'nw (* gap-size -3) (* gap-size -3))]
        [p (pin-balloon b3 p verbs-item rc-find)]
        [meth?-p p]
        [p (pin-over p obj?-label rb-find obj-obj)]
        [obj-obj-p p]
        [p (pin-over p meth?-label rb-find meth-obj)]
        [meth-obj-p p])
   (list (list plain-p)
         (list 'alts~
               (map list (list obj?-p
                               meth?-p
                               obj-obj-p
                               meth-obj-p))))))

(define abstraction-item
  (item "Enable syntactic abstraction"))

(define provide-constructs-item
  (item "Provide expressive constructs"))

(define easy-abstraction-item
 (item "Make syntactic abstraction" (it "easy")))

(define smooth-item
  (item "Smooth the path from syntactic abstraction to language construction"))

(define dsl-title "Domain-Specific Programming")

(define (lt s) (colorize (bt s) "forestgreen"))

(slide
 #:title dsl-title
 (para "The" (lt "programming language") "approach:")
 provide-constructs-item
 (ghost abstraction-item)
 (ghost easy-abstraction-item)
 (ghost smooth-item))

(slide
 #:title dsl-title
 (para "The" (lt "Lisp") "approach:")
 provide-constructs-item
 abstraction-item
 (ghost easy-abstraction-item)
 (ghost smooth-item))

(slide
 #:title dsl-title
 (para "The" (lt "Scheme") "approach:")
 provide-constructs-item
 abstraction-item
 easy-abstraction-item
 (ghost smooth-item))

(slide
 #:title dsl-title
 (para "The" (lt "Racket") "approach:")
 provide-constructs-item
 abstraction-item
 easy-abstraction-item
 smooth-item)

(slide
 #:title motivation-title
 (scale
  (code
   (define-verbs all-verbs
     [north (n) "go north"]
     [get _ (grab take) "take"]
     ....)
   code:blank
   (define-actions everywhere-actions
     ([quit (printf "Bye!\n") (exit)]
      [look (show-current-place)]
      ....))
   code:blank
   (define-thing cactus
     [get "Ouch!"])
   ....
   code:blank
   (define-place desert
     "You're in a desert. There is nothing for miles around."
     (cactus key)
     ([north start]
      [south desert] ....))
   ....)
  0.8))

(define (verbatim . strs)
  (apply vl-append
         (let loop ([line null] [strs strs])
           (cond
            [(null? strs)
             (loop line '("\n"))]
            [(equal? (car strs) "\n")
             (cons (if (null? line)
                       (tt " ")
                       (tt (apply string-append (reverse line))))
                   (if (null? (cdr strs))
                       null
                       (loop null (cdr strs))))]
            [else
             (loop (cons (car strs) line) (cdr strs))]))))

(slide
 #:title motivation-title
 (scale
  (code
   #,(tt "===VERBS===")
   #,(hbl-append (code north) (tt ", ") (code n))
    "go north"
   ....
   #,(tt "===EVERYWHERE===")
   quit 
    (begin (printf "Bye!\n") (exit))
   ....
   #,(tt "===THINGS===")
   ---cactus---
   get
    "Ouch!"
   ....
   #,(tt "===PLACES===")
   ---desert---
   "You're in a desert. There is nothing for miles around."
    [#,(hbl-append (code cactus) (tt ", ") (code key))]
   north   start
   south   desert
   ....)
  0.8))

;; ----------------------------------------

(what-is-a-macro-slides)

(demo-slide #:hack? #t "game implementation overview")

;; ----------------------------------------

(simple-pattern-slides)
(pattern-slides)
(pattern-...-slides)

(demo-slide #:hack? #t "complete game implementation")

(lexical-slides)
(lexical-how-slides)

(demo-slide #:hack? #t "modular game implementation")

;; ----------------------------------------

(implicit-slides)

(demo-slide #:hack? #t "game module language")

;; ----------------------------------------

(define-syntax-slides)
(rep-code-slides)
(phase-slides)
(syntax-case-slides)

(demo-slide #:hack? #t "``typed'' game language")

;; ----------------------------------------

(parsing-slides)

(demo-slide #:hack? #t "non-S-expression game language")

;; ----------------------------------------

(slide
 #:title "Environment Support"
 (para "Support at S-expression level is free")
 (item "Error source locations")
 (item "Check Syntax")
 (blank)
 (para "Source-editing support requires more")
 (item "On-the-fly coloring"))

(demo-slide "DrRacket editor support")

;; ----------------------------------------

(slide
 #:title "Languages in Racket"
 pipeline-pict)
