#lang slideshow
(require slideshow/play
         slideshow/code
         racket/math
         "color.rkt"
         "history.rkt"
         "asm.rkt")

(provide chez-slides)

(define stack-h 600)
(define block-w 300)
(define block-sep 2)

(define block-frame-size 1)

(define bottom-labels? #f)

(define racket7-final? #t)
(define rumble-label? #f)

(define Racket-on-Chez-name "Racket on Chez Scheme")

;; Racket 6.10.1.3
;;  C in src+foreign+gc: 219k [+libffi and other little pieces]
(define racket-runtime-size 234)
;;              gc: 11k
(define racket-runtime-size/gc 11)
;;              fun.c + struct.c: 17k
(define racket-runtime-size/fun 11)
;;              thread + sema + future: 15k
(define racket-runtime-size/thread 15)
;;              port + portfun + network: 15k
(define racket-runtime-size/io 15)
(define racket-runtime-size/rktio 14)
;;              regexp: 6k
(define racket-runtime-size/regexp 6)
;;              expander + reader: 35k [30k in about 6 files, but delta with v6 is 35k]
(define racket-runtime-size/expander 35)
;;              compiler: 25k [estimate from compile.c + jit*.c]
(define racket-runtime-size/compiler 23)
;;  Racket for distribution: 885k
(define racket-distro-size 885)
;;           collects: 150k
(define racket-collects-size 150)
;;           collects: 150k
(define racket-docs-size 270)
;;           .scrbl: 270

;; Chez Scheme [maybe 1/4 is redundant]
;;  C: 18k [+libz]
(define chez-scheme-kernel-size 18)
(define chez-scheme-kernel-size/gc 2)
;;  Scheme: 97k [72 files]
(define chez-scheme-size 97)
;;     compiler: 30k
;;       [= cpnanopass.ss cp0.ss x86.ss x86_64.ss arm32.ss np-languages.ss base-lang.ss cpletrec.ss cpvalid.ss]
(define chez-scheme-size/compiler 30)
;;     expander & reader: 8k
;;       [first part of "syntax.ss" plus "read.ss"]
(define chez-scheme-size/expander 8)

;; Racket7
;;  C in src+foreign+gc: 183k [delta = 37k]
;;  Racket:
;;    expander: 30k [4k is reader; about 225 files]
;;       [compare to 6k for Chez Scheme "syntax.ss" first half]
;;       [99k lines expanded + Schemified]
(define expander-size 30)
;;    linklet: 2k
(define r7-linklet-size 2)

;; Racket-on-Chez:
;;  Scheme:
;;    rumble: 15k [hash tables, control, FFI, struct, chaperones & impersonators]
(define rumble-size 15)
;;  Racket:
;;    expander: 30k [4k is reader]
;;    thread: 6.7k [65 files]
(define thread-size 7)
;;    io: 15k [> 155 files]
(define io-size 15)
;;    regexp: 4k [31 files]
(define regexp-size 4)
;;    schemify: 5k
(define schemify-size 5)
;;    linklet: 2k
(define linklet-size 2)

(define size-sep 5)

(define (stack l
               #:height [height (stack-height l)]
               #:adjust [adjust (lambda (pt p) p)]
               #:and-blocks? [and-blocks? #f])
  (define h (stack-height l))
  (define blocks (for/list ([pt (in-list l)])
                   (adjust pt ((block height block-w) pt))))
  (define block-stack (if (= (length blocks) 1)
                          (car blocks)
                          (apply vc-append blocks)))
  (define together
    (apply vc-append
           (if (h . < . height)
               (cons (ghost ((block height block-w)
                             (part #:size (- height h))))
                     (list block-stack))
               (list block-stack))))
  (if and-blocks?
      (values together blocks)
      together))

(define (stack-height l)
  (apply + (map part-height l)))

(define ((block height block-w) spec)
  (define h (* stack-h (/ (part-height spec) height)))
  (define color (part-color spec))
  (define cell (linewidth
                block-frame-size
                (frame (cellophane
                        (colorize (filled-rectangle block-w (max 1 (- h block-sep block-sep)))
                                  (part-color spec))
                        0.5)
                       #:color color)))
  (define lbl (inset (tt (cond
                           [(equal? color c-color) ".c"]
                           [(equal? color racket-color) ".rkt"]
                           [(equal? color scheme-color) ".scm"]
                           [else "??"]))
                     0 (- 5)))
  (define cell/label
    (if ((pict-height cell) . > . (pict-height lbl))
        (refocus (hc-append gap-size cell lbl) cell)
        cell))
  (inset cell/label 0 block-sep))

(define (part #:name [name ""]
              #:name-pict [name-pict #f]
              #:size size
              #:color [color racket-color]
              #:size-dy [size-dy 0]
              #:label-overflow [label-overflow (if bottom-labels? 'left #f)])
  (hash 'name name 'name-pict name-pict
        'size size 'color color
        'size-dy size-dy 'label-overflow label-overflow))

(define (part-height p)
  (hash-ref p 'size))

(define (part-name p)
  (hash-ref p 'name))

(define (part-name-pict p)
  (hash-ref p 'name-pict))

(define (part-color p)
  (hash-ref p 'color))

(define (part-size-dy p)
  (hash-ref p 'size-dy))

(define (part-label-overflow p)
  (hash-ref p 'label-overflow))

(define (part->hilite-part p)
  (hash-set p 'hilite #t))

(define (scale-dy dy) (* dy (/ stack-h 678)))

(define racket-runtime-size/other
  (- racket-runtime-size
     racket-runtime-size/gc
     racket-runtime-size/fun
     racket-runtime-size/thread
     racket-runtime-size/io
     racket-runtime-size/rktio
     racket-runtime-size/regexp
     racket-runtime-size/expander
     racket-runtime-size/compiler))

(define rktio
  (part #:name "rktio" #:name-pict (tt "rktio") #:size racket-runtime-size/rktio #:color c-color))

(define expand/read/module "expander")
(define (string-append/maybe a b)
  (if bottom-labels?
      (string-append a b)
      b))

(define c-expander (part #:name (string-append/maybe "Racket " expand/read/module) #:size racket-runtime-size/expander #:color c-color))

(define racket-v6-stack
  (list c-expander
        (part #:name "regexp" #:size racket-runtime-size/regexp #:color c-color #:label-overflow (and bottom-labels? 'right))
        (part #:name "I/O" #:size racket-runtime-size/io #:color c-color)
        rktio
        (part #:name "threads" #:size racket-runtime-size/thread #:color c-color)
        (part #:name "control+structs" #:size racket-runtime-size/fun #:color c-color)
        (part #:name "builtins" #:size racket-runtime-size/other #:color c-color)
        (part #:name "bytecode+JIT compiler" #:size racket-runtime-size/compiler #:color c-color)
        (part #:name "gc" #:size racket-runtime-size/gc #:color c-color)))

(define racket-expander (part #:name (string-append/maybe "Racket " expand/read/module) #:size expander-size))
(define racket-linklet (part #:name "linklet" #:size r7-linklet-size #:color c-color #:size-dy (scale-dy -8)
                             #:label-overflow (and bottom-labels? 'right)))

(define racket-v7-stack
  (list* racket-expander
         racket-linklet
         (cdr racket-v6-stack)))

(define racket-v7 (stack #:height (stack-height racket-v6-stack)
                         racket-v7-stack))

(define chez-scheme-size/other
  (- chez-scheme-size
     chez-scheme-size/expander
     chez-scheme-size/compiler))

(define chez-kernel-part
  (part #:name "kernel" #:size chez-scheme-kernel-size #:color c-color))

(define chez-scheme-kernel-size/other
  (- chez-scheme-kernel-size
     chez-scheme-kernel-size/gc))

(define chez-kernel-stack
  (list (part #:name "kernel" #:size chez-scheme-kernel-size/other #:color c-color)
        (part #:name "gc" #:size chez-scheme-kernel-size/gc #:color c-color)))

(define racket-v6-core (stack racket-v6-stack))

;; ----------------------------------------

(define chez-compiler-part
  (part #:name "compiler+builtins" #:size chez-scheme-size #:color scheme-color))

(define chez-scheme-stack
  (list chez-compiler-part
        chez-kernel-part))

;; ----------------------------------------

(define linklet
  (part #:name "linklet" #:color scheme-color #:size linklet-size #:size-dy (scale-dy -9)))

(define sources+rktio+linklet
  (list
   racket-expander
   linklet
   (part #:name "schemify" #:size schemify-size #:size-dy (scale-dy -3))
   (part #:name "regexp" #:size regexp-size #:size-dy (scale-dy -2))
   (part #:name "I/O" #:size io-size)
   rktio
   (part #:name "threads" #:size thread-size)))

(define (make-racket-on-chez-stack chez-scheme-stack)
  (append
   (list
    (part #:name "main" #:size 1 #:color scheme-color #:size-dy (scale-dy -10)))
   sources+rktio+linklet
   (list
    (part #:name (if rumble-label? "control+structs / rumble" "control+structs+...") #:size rumble-size #:color scheme-color))
   chez-scheme-stack))

(define racket-on-chez-stack (make-racket-on-chez-stack chez-scheme-stack))

;; ----------------------------------------

(define (chez-slides)
  (define use-height (stack-height racket-v6-stack))

  (define rkt-on-chez-title
    (ca 2019 "Racket on Chez Scheme"))
  (define rkt-on-chez-name "Racket on Chez Scheme")

  (define chez-title
    (ca "1984-2019" "Chez Scheme" #:who "Dybvig et al."))
  (define chez-name "Chez Scheme")

  (define nanopass-title
    (ca 2012 "Nanopass" #:who "Keep"))
  (define nanopass-name "Nanopass")

  (as-history
   #:res 0
   #:edu 0
   
   (play-n
    #:title rkt-on-chez-title
    #:name rkt-on-chez-name
    #:skip-last? #t
    (lambda (n)
      (stack racket-v6-stack
             #:adjust (lambda (pt p)
                        (cond
                          [(eq? pt c-expander)
                           (define dy (* client-h (fast-end n)))
                           (inset p 0 dy 0 (- dy))]
                          [else p])))))

   (play-n
    #:title rkt-on-chez-title
    #:name rkt-on-chez-name
    #:skip-first? #t
    (lambda (n)
      (stack racket-v7-stack
             #:height use-height
             #:adjust (lambda (pt p)
                        (cond
                          [(or (eq? pt racket-expander)
                               (eq? pt racket-linklet))
                           (define dy (* client-h 1/2 (- 1 (fast-end n))))
                           (inset p 0 (- dy) 0 dy)]
                          [else p])))))

   (define-values (expander-dy rktio-dy)
     (let-values ([(rv7 rv7-blocks) (stack racket-v7-stack
                                           #:height use-height
                                           #:and-blocks? #t)]
                  [(rcs rcs-blocks) (stack racket-on-chez-stack
                                           #:height use-height
                                           #:and-blocks? #t)])
       (define (find pt stack block-stack blocks)
         (cond
           [(null? stack) (values #f #f)]
           [(eq? pt (car stack))
            (lt-find block-stack (car blocks))]
           [else
            (find pt (cdr stack) block-stack (cdr blocks))]))
       (define (delta pt)
         (define-values (r7x r7y) (find pt racket-v7-stack rv7 rv7-blocks))
         (define-values (csx csy) (find pt racket-on-chez-stack rcs rcs-blocks))
         (- csy r7y))
       (values (delta racket-expander)
               (delta rktio))))

   (play-n
    #:title rkt-on-chez-title
    #:name rkt-on-chez-name
    #:skip-first? #t
    (lambda (n)
      (stack racket-v7-stack
             #:height use-height
             #:adjust (lambda (pt p)
                        (define dy
                          (cond
                            [(eq? pt racket-expander)
                             (* expander-dy (fast-middle n))]
                            [(eq? pt rktio)
                             (* rktio-dy (fast-middle n))]
                            [else
                             (* client-h (fast-end n))]))
                        (inset p 0 dy 0 (- dy))))))

   (play-n
    #:title rkt-on-chez-title
    #:name rkt-on-chez-name
    #:skip-first? #t
    (lambda (n)
      (cb-superimpose
       (stack racket-on-chez-stack
              #:height use-height
              #:adjust (lambda (pt p)
                         (if (or (eq? pt racket-expander)
                                 (eq? pt rktio))
                             p
                             (ghost p))))
       (stack chez-scheme-stack
              #:height use-height
              #:adjust (lambda (pt p)
                         (let ([p (if (and (= n 1)
                                           (eq? pt chez-compiler-part))
                                      (refocus (hc-append (* 2 gap-size)
                                                          (bt "Chez Scheme")
                                                          p)
                                               p)
                                      p)])
                           (define dy (* client-h (fast-end (- 1 n))))
                           (inset p 0 dy 0 (- dy))))))))

   (play-n
    #:title rkt-on-chez-title
    #:name rkt-on-chez-name
    #:skip-first? #t
    (lambda (n)
      (define all
        (stack racket-on-chez-stack
               #:height use-height
               #:adjust (lambda (pt p)
                          (cond
                            [(or (memq pt chez-scheme-stack)
                                 (memq pt racket-v7-stack))
                             p]
                            [else
                             (define dx (* client-w (- 1 (fast-middle n))))
                             (inset p (- dx) 0 dx 0)]))))
      (if (= n 1)
          (refocus (refocus (hc-append (* 2 gap-size)
                                       (vc-append
                                        (current-line-sep)
                                        (bt "Racket on")
                                        (bt "Chez Scheme"))
                                       all)
                            all)
                   all)
          all)))
   
   (play-n
    #:title chez-title
    #:name chez-name
    #:skip-first? #t
    #:skip-last? #t
    (lambda (n)
      (stack racket-on-chez-stack
             #:height use-height
             #:adjust (lambda (pt p)
                        (if (eq? pt chez-compiler-part)
                            p
                            (cellophane p (- 1 n))))))))

  (define step-color "gray")
  (define new-step-color "gold")
  (define arrow-size 16)
  (define arrow-color "purple")
  (define arrow-line-width 3)

  (define (add-inside step inside #:superimpose [superimpose rb-superimpose])
    (refocus (superimpose step
                          (cc-superimpose
                           (colorize (filled-rectangle (max (pict-width inside)
                                                            (pict-width step))
                                                       (max (pict-height inside)
                                                            (pict-height step)))
                                     step-color)
                           inside))
             step))

  (define (nanopass #:sprout-n [sprout-n 0])
    (define b&w-step (filled-rectangle 70 50))
    (define step (colorize b&w-step step-color))
    (define step-sep (* 1/3 (pict-height step)))
    (define (sprout-new p sprout-n)
      (if (and sprout-n (sprout-n . > . 0))
          (vc-append (scale
                      (let ([q (inset (colorize b&w-step new-step-color) 0 0 0 step-sep)])
                        (pin-arrow-line arrow-size
                                        q
                                        b&w-step cb-find
                                        q cb-find
                                        #:color arrow-color
                                        #:line-width arrow-line-width))
                      1 sprout-n)
                     p)
          p))
    (define (column n
                    #:start-inside [start-inside (blank)]
                    #:end-inside [end-inside (blank)]
                    #:sprout-top-n [sprout-top-n #f])
      (for/fold ([p (sprout-new (add-inside step start-inside) sprout-top-n)]) ([i n])
        (define new-step (if (= i (sub1 n))
                             (add-inside step end-inside #:superimpose lt-superimpose)
                             (inset step 0)))
        (define new-p (vc-append step-sep
                                 p
                                 new-step))
        (pin-arrow-line arrow-size
                        new-p
                        p cb-find
                        new-step ct-find
                        #:color arrow-color
                        #:line-width arrow-line-width)))
    (define-values (grid last-column)
      (let ([p (column 5 #:start-inside (code (λ (x) x)))])
        (for/fold ([p p] [prev p]) ([i 4])
          (define new-column (column (case i
                                       [(1) 4]
                                       [(2) 5]
                                       [else 6])
                                     #:sprout-top-n
                                     (case i
                                       [(1) sprout-n]
                                       [else #f])
                                     #:end-inside
                                     (case i
                                       [(3) (inset (scale asm-code 0.5) 6)]
                                       [else (blank)])))
          (define new-p (hc-append (pict-width new-column)
                                   p
                                   new-column))
          (values (pin-arrow-line arrow-size
                                  new-p
                                  prev cb-find
                                  new-column ct-find
                                  #:start-angle (* -1/3 pi)
                                  #:end-angle (* -1/3 pi)
                                  #:start-pull 0.3
                                  #:end-pull 0.3
                                  #:color arrow-color
                                  #:line-width arrow-line-width)
                  new-column))))
    grid)

  (define nanopass-start (nanopass))

  (as-history
   (play-n
    #:title chez-title
    #:name chez-name
    #:skip-last? #t
    (let ([purple-box (stack racket-on-chez-stack
                             #:height use-height
                             #:adjust (lambda (pt p)
                                        (if (eq? pt chez-compiler-part)
                                            p
                                            (ghost p))))])
      (lambda (n)
        (let ([n (fast-middle n)])
          (cc-superimpose
           (let ([pb (scale (ghost purple-box) (max 0.1 (- 1 n)))])
             (refocus (cc-superimpose
                       (launder
                        (cellophane (scale (inset purple-box
                                                  0 (* n -1/2 client-h) 0 0)
                                           (+ 1 (* n 10)))
                                    (- 1 n)))
                       pb)
                      pb))
           (if (zero? n)
               (blank)
               (leftward (inset (scale nanopass-start n)
                                0 (* (- 1 n) 1/4 client-h) 0 0)))))))))

  (as-history
   #:res 2
   (play-n
    #:title nanopass-title
    #:name nanopass-name
    (lambda (n)
      (leftward (nanopass #:sprout-n n)))))

  (void))

(module+ main
  (chez-slides))
