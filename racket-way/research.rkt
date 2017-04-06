#lang slideshow
(require slideshow/code
         "redex-ex.rkt"
         "../killsafetalk/subproc.rkt"
         "unit.rkt"
         "as-file.rkt"
         "blueprint.rkt"
         "window.rkt"
         "hat.rkt")

(provide research-slides)

(define (research-slides)
  (define (group m . l)
    m
    #;
    (apply vc-append
           (* 2 (current-line-sep))
           m
           l))

  (define (pub who where when)
    (parameterize ([current-font-size (floor (* #e0.75 (current-font-size)))])
      (para #:align 'right who " " (hbl-append (t where) (t " ") (t when)))))

  (define (spacer) (tt " "))

  (define (pubs . l)
    (apply vc-append (/ gap-size 2) l))

  (define (space p) (inset p 0 gap-size))

  (define (subalt model book p)
    (let ([sel (lambda (a b) (cc-superimpose a (ghost b)))])
      (list (list (vc-append gap-size (sel model book) p))
            (list (vc-append gap-size (sel book model) p)))))

  (define (fn s)
    (scale (tt s) 0.7))

  (define (icon+title icon s)
    (hbl-append icon (titlet (string-append " " s))))

  (define (dip p)
    (lift-above-baseline p (* -0.2 (pict-height p))))

  (define spec-icon (dip (scale blueprint 0.25)))
  (define env-icon (dip window))
  (define formal-icon (dip hat))

  (slide
   #:title "Research Questions"
   (group
    (item #:bullet spec-icon "How can we help programmers define languages?")
    (subitem "domain-specific languages (DSLs)")
    (subitem "entirely new languages"))
   (blank)
   (group
    (item #:bullet env-icon "How can an environment adapt to new languages?")
    (subitem "editing, browsing, and refactoring tools")
    (subitem "documentation tools"))
   (blank)
   (group
    (item #:bullet formal-icon "How do we reason about new languages?")
    (subitem "formal semantics")
    (subitem "static analysis")
    (subitem "safety")))

  (slide
   #:title (icon+title spec-icon "Extending and Defining Languages")
   'alts
   (list
    (list
     (para "Lisp-style macros")
     'alts
     (subalt
      (space
       (scale
        (code (define-thing door
                [open (begin
                        (set-thing-state! door 'open)
                        "The door is now unlocked and open.")]
                [close (begin
                         (set-thing-state! door #f)
                         "The door is now closed.")]
                [knock "No one is home."]))
        0.7))
      (space
       (scale
        (code 
         (define-syntax-rule (define-thing id 
                               [vrb expr] ...)
           (begin
             (define id 
               (thing 'id #f 
                      (list (cons vrb (lambda () expr))
                            ...)))
             (record-element! 'id id))))
        0.7))
      (pubs
       (spacer)
       (pub "Atkinson, Flatt, Lindstrom" "GPCE" "2010")
       (pub "Tobin-Hochstadt, St-Amour, Culpepper, Flatt, Felleisen" "PLDI" "2011")
       (pub "Flatt, Culpepper, Darais, Findler" "JFP" "2012")
       (pub "Rafkind, Flatt" "GPCE" "2012"))))
    (list
     (para "Syntax-aware module system")
     'alts
     (subalt
      (space
       (hc-append (* 3 gap-size)
                  (as-file (fn "txtadv.rkt")
                           (scale (code 
                                   #,(tt "#lang racket")
                                   code:blank
                                   (provide define-thing)
                                   code:blank
                                   (define-syntax define-thing
                                     ...))
                                  0.7))
                  (as-file (fn "game.rkt")
                           (scale (code 
                                   #,(tt "#lang racket")
                                   code:blank
                                   (require "txtadv.rkt")
                                   code:blank
                                   (define-thing door
                                     ...))
                                  0.7))))
      (space
       (hc-append (* 3 gap-size)
                  (as-file (fn "txtadv.rkt")
                           (scale (code 
                                   #,(tt "#lang racket")
                                   code:blank
                                   ...
                                   code:blank
                                   (module lang ...
                                     ...))
                                  0.7))
                  (as-file (fn "game.rkt")
                           (scale (code 
                                   #,(tt "#lang txtadv   ")
                                   code:blank
                                   #,(tt "---door---")
                                   code:blank
                                   #,(tt "open")
                                   ...
                                   #,(tt "close")
                                   ...)
                                  0.7))))
      (pubs
       (pub "Flatt" "ICFP" "2002")
       (pub "Flatt" "Queue/CACM" "2011"))))
    (list
     (para "Process abstractions")
     (space (scale subproc-web-server 0.75))
     (pubs
      (pub "Flatt, Findler, Krishnamurthi, Felleisen" "ICFP" "1999")
      (pub "Flatt, Findler" "PLDI" "2004")
      (pub "Tew, Swaine, Flatt, Findler, Dinda" "DLS" "2011")))))

  (slide
   #:title (icon+title env-icon "Programming Environments")
   'alts
   (list
    (list
     (para "DrRacket")
     (bitmap "drracket.png")
     (pub "Findler, Clements, Flanagan, Flatt, Krishnamurthi, Steckler, Felleisen" "JFP" "2002"))
    (list
     (para "Scribble and Slideshow")
     (space (bitmap "../scribble/talk/drracket.png"))
     (pubs
      (pub "Findler, Flatt" "JFP" "2006")
      (pub "Flatt, Barzilay, Findler" "ICFP" "2009")))))

  (slide
   #:title (icon+title formal-icon "Formal Models")
   'alts
   (list
    (list
     (para "Redex")
     'alts
     (subalt 
      (space (frame (inset (scale (hc-append gap-size iswim-pict !->v-pict) 1.75) gap-size)))
      (space (scale (bitmap "sewpr.png") 2/3))
      (pubs
       (pub "Felleisen, Findler, Flatt" "MIT Press" "2009")
       (pub "Klein, Clements, Dimoulas, Eastlund, Felleisen, Flatt, McCarthy, Rafkind, Tobin-Hochstadt, Findler"
            "POPL"
            "2012")
       (pub "Klein, Flatt, Findler" "HOSC" "2012"))))
    (list
     (para "Modularity and interoperability")
     (space (scale example-unit 0.75))
     (pubs
      (pub "Flatt, Felleisen" "PLDI" "1998")
      (pub "Findler, Flatt" "ICFP" "1998")
      (pub "Gray, Findler, Flatt" "OOPSLA" "2005")
      (pub "Owens, Flatt" "ICFP" "2006")
      (pub "Strickland, Tobin-Hochstadt, Findler, Flatt" "OOPSLA" "2012")))))

  ;; ----------------------------------------

  (slide
   #:title "Research and Teaching"
   (bitmap "../cs2010/f11/htdp-cover.gif")))

;; ----------------------------------------

(module+ main
  (research-slides))
