#lang slideshow
(require slideshow/balloon
         slideshow/code
         "lightbulb.rkt"
         "style.rkt"
         "in-file.rkt"
         "config.rkt")

(provide use-lexical-scope-slides
         use-lex-scope-balloon)

(define (reason desc space p l r)
  (list
   (blank)
   desc
   (blank)
   p
   'next
   (blank)
   r))

(define title
  "Use Scope")

(define use-lex-scope-balloon
  (balloon-pict
   (wrap-balloon
    (hc-append
     gap-size
     (scale (bright-lightbulb) 1/2)
     (titlet title))
    's 0 0)))

(define (use-lexical-scope-slides #:submod? [submod? #t]
                                  #:escapes? [escapes? #f]
                                  #:extra-content [extra-content null])
  (slide
   #:title use-lex-scope-balloon
   #:name title
   'next
   (blank)
   'alts
   (append
    (list
     (reason
      (para "Scope is declarative")
      (* 3 gap-size)
      (let ([x1 (code x)]
            [x2 (code x)])
        (pin-arrow-line (/ gap-size 2)
                        (code
                         (let ([#,(hilite x1 #:style 'ellipse) 5])
                           #,(hilite x2 #:style 'ellipse)))
                        x1 lc-find
                        x2 rc-find
                        #:solid? #f
                        #:line-width 3
                        #:color runtime-color))
      (code (define-syntax or
              ....
              #'(let ([x e1]) (if x x e2))))
      (mk-file #:name "main"
               #:suffix "rkt"
               (scale
                (code
                 (require "list.rkt"
                          "grocery.rkt")
                 (shop (fold (groceries ....))))
                0.75)))
     (reason
      (para "Binding implies availability")
      (* 4 gap-size)
      (code
       (let ([x 5])
         (+ x x)))
      (code
       (define-syntax grocery ....)
       (grocery ....))
      (mk-file #:name "grocery"
               #:suffix "rkt"
               (scale
                (code
                 (require "gui.rkt")
                 .... (new frame%) ....)
                0.9))))
    (if submod?
        (list
         (reason
          (para "Scope need not imply dynamic extent")
          (* 4 gap-size)
          (code
           (lambda (x)
             (lambda (y)
               (+ x y))))
          (code (define-syntax-rule (or e1 e2)
                  (let ([x e1]) (if x x e2))))
          (mk-file #:name "program"
                   #:suffix "rkt"
                   (code (module test ....)))))
        null)
    (if (not escapes?)
        null
        (list
         (reason
          (para "Escape hatches are available")
          (* 3 gap-size)
          (scale
           (code
            (parameterize ([current-output-port
                            (open-log-file)])
              ....))
           0.75)
          (code
           (datum->syntax ....))
          (mk-file #:name "gui"
                   #:suffix "rkt"
                   (scale
                    (code
                     (dynamic-require
                      (case (system-type)
                        [(unix) "gtk.rkt"]
                        [(macosx) "cocoa.rkt"]
                        [(windows) "win32.rkt"])
                      ....))
                    0.75)))))
    (for/list ([e (in-list extra-content)])
      (list (blank) e)))))

(module+ main
  (use-lexical-scope-slides))
