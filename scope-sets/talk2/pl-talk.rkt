#lang at-exp slideshow
(require slideshow/code
         slideshow/balloon
         "scope.rkt"
         "terminology.rkt"
         "pre.rkt")

(slide
 (titlet "Introduction to Research in Programming Languages"))

(balloon-enable-3d #f)

(define fortran-example
  @pre{
    * IOCS(DISK,TYPEWRITER,KEYBOARD,PAPERTAPE)
          DIMENSION IEMG(10,15),IEMG1(13)
          DEFINE FILE 12(80,150,U,K)
          WRITE(1,10)
       10 FORMAT('PAPERTAPE'//'GIVE NUMBER')
          READ(6,30) M
       30 FORMAT(I1)
          PAUSE 1
          DO 25 N=1,16
          DO 15 I=1,15
          READ(4,20) IEMG1
       })

(define lisp-example
  @pre{
   (MAPLIST (LAMBDA (L FN) (COND ((NULLL) NIL)
       (T (CONS (FN L) (MAPLIST (CDR L) FN))))))
   (CHANGE (LAMBDA (A) (MAPLIST A (FUNCTION
     (LAMBDA ( J ) (CONS (CAR J ) (QUOTE X))) ))))
   DEFINE ((
    (FACTORIAL (LAMBDA (N) (COND
       ((ZEROPN) 1)
       (T (TIMES N (FACTORIAL (SUB1 N)))) )))
   ))
  })

(define algol60-example
  @pre{
    begin
      integer procedure SIGMA(x, i, n);
        value n;
        integer x, i, n;
      begin
        integer sum;
        sum:=0;
        for i:=1 step 1 until n do
            sum:=sum+x;
        SIGMA:=sum;
      end;
      integer q;
      printnln(SIGMA(q*2-1, q, 7));
    end
  })

(define c-example
  @pre{
    int fact(int n) {
     if (n == 0)
       return 1;
     else
       return n * fact(n-1);
    }
   })

(define smalltalk-example
  @pre{
    | names result |
        names := Collection with: 'John'.
        result := names add: 'Wayne'.
   })

(define ml-example
  @pre{
    fun naive_fib 0 = 0
      | naive_fib 1 = 1
      | naive_fib n = naive_fib(n-2)
                      + naive_fib(n-1);
  })

(define java-example1
  @pre{
    interface List {
      int length();
      Object visit(ListVisitor v);
    }
  })

(define java-example2
  @pre{
    class Cons extends List {
      Object first;
      List rest;
      int length() { return 1+rest.length@";" }
      Object visit(ListVisitor v) {
        return v.cons(first, rest.visit(v));
      }
    }
  })

(define java-example3
  @pre{
    class Empty extends List {
      int length() { return 0@";" }
      Object visit(ListVisitor v) {
        return v.null();
      }
    }
  })

(define haskell-example
  @pre{
   split :: String -> Char -> [String]
   split "" _ = []
   split xs c = let (ys, zs) = break (== c) xs
                in  if null zs 
                       then [ys] 
                       else ys : split (tail zs) c
  })

(define javascript-example
  @pre{
   function noteFilterChanged() {
     var box = { latest : true };
     prev_box.latest = false;
     prev_box = box;
     setTimeout(function (){
                  if (box.latest) filterChanged(); 
                }, 200);
   }
 })

(define racket-example
  (code
   #,(tt "#lang") racket
   (require 2htdp/image)
   code:blank
   (let sierpinski ([n 8])
     (cond
      [(zero? n) (triangle 2 'solid 'red)]
      [else (define t (sierpinski (- n 1)))
            (freeze (above t (beside t t)))]))))

;; ----------------------------------------

(define (example p #:scale [s 0.45])
  (define i (inset (scale p s) (/ gap-size 2)))
  (frame (cc-superimpose
          (colorize (filled-rectangle (pict-width i) (pict-height i))
                    "beige")
          i)))

(define (lt s) (colorize (bt s) "brown"))
  
(slide
 #:title "1950s"
 (para #:align 'center "In the beginning, there was" (lt "Fortran") "and" (lt "Lisp"))
 (blank)
 (blank)
 (ht-append (* 3 gap-size)
            (example fortran-example)
            (example lisp-example))
 'next
 (blank)
 (blank)
 (colorize (para #:align 'center "(Some things never change)") "mediumblue"))

(slide
 #:title "1960s"
 (lt "Algol")
 (example algol60-example)
 'next
 (item "BNF Grammars")
 'next
 (item "Dominant syntactic style")
 'next
 (vl-append
  (current-line-sep)
  (item "First steps for many modern concepts")
  (para #:align 'right "lexical scope, type systems, and laziness")))

(define dots (let ([c (filled-ellipse 5 5)])
               (vc-append 10 c c c)))

(define type-rule
  (let ([top (rt "Γ[x:τ₁] ⊢ e : τ₂")]
        [bottom (rt "Γ ⊢ λx:τ₁.e : τ₁→τ₂")])
    (vc-append (current-line-sep)
               top
               (hline (max (pict-width top) (pict-width bottom)) 0)
               bottom)))

(define (ct s) (colorize (it s) "blue"))

(define (70s-90s-slide #:90s? [90s? #f]
                       #:conference [conference #f]
                       #:conferences? [conferences? (or 90s? conference)]
                       #:types? [types? #f])
  (define (maybe-pin-balloon p which b)
    (if (eq? which conference)
        (refocus (pin-balloon b p p cb-find) p)
        p))
  (define java (lt "Java"))
  (define (or-java p)
    (if 90s?
        (cbl-superimpose java (ghost p))
        (cbl-superimpose (ghost java) p)))
  (define cv (if conferences? values ghost))
  (define (sel-example p j superimpose)
    (let* ([j (example j)]
           [p (ct-superimpose (example p)
                              (blank 0 (pict-height j)))])
    (if 90s?
        (refocus (superimpose (ghost p) j) p)
        p)))
  (slide
   #:title (if 90s? "1990s" "1970s-1980s")
   (let ([p (table
             3
             (list (or-java (lt "C"))
                   (or-java (lt "Smalltalk"))
                   (or-java (para #:fill? #f (lt "ML") "," (lt "Haskell") "," (lt "Scheme")))
                   (sel-example c-example java-example1 lt-superimpose)
                   (sel-example smalltalk-example java-example2 (lambda (p p2) (lt-superimpose p (inset p2 gap-size 0 0 0))))
                   (sel-example ml-example java-example3 rt-superimpose)
                   (t "imperative") (t "object-oriented") (t "functional")
                   (cv dots) (cv dots) (cv dots)
                   (maybe-pin-balloon (cv (ct "PLDI"))
                                      'pldi
                                      (wrap-balloon (vl-append
                                                     (current-line-sep)
                                                     (ct "Programming Languages Design and Implementation")
                                                     (t "compilers"))
                                                    'nw 0 (- gap-size)))
                   (maybe-pin-balloon (cv (ct "OOPLSA"))
                                      'oopsla
                                      (wrap-balloon (vl-append
                                                     (current-line-sep)
                                                     (ct "Object-Oriented Programming, Languages, Systems, and Applications")
                                                     (t "OO and runtime systems"))
                                                    'n 0 (- gap-size)))
                   (cv (hbl-append (maybe-pin-balloon (ct "POPL")
                                                      'popl
                                                      (wrap-balloon
                                                       (vc-append
                                                        (current-line-sep)
                                                        (ct "Principle of Programming Languages")
                                                        (t "formal approaches"))
                                                       'ne 0 (- gap-size)))
                                   (t " and ")
                                   (maybe-pin-balloon (ct "ICFP")
                                                      'icfp
                                                      (wrap-balloon (vl-append
                                                                     (current-line-sep)
                                                                     (ct "International Conference on Functional Programming")
                                                                     (t "functional programming"))
                                                                    'ne 0 (- gap-size))))))
             ctl-superimpose (list* ctl-superimpose ct-superimpose ctl-superimpose)
             (* 1.5 gap-size) gap-size)])
     (if types?
         (refocus (vc-append (* 2 gap-size)
                             p
                             (hc-append gap-size
                                        (colorize (scale (para #:fill? #f "... and" (bit "types")) 1.4) "forestgreen")
                                        type-rule))
                  p)
         p))))

(70s-90s-slide)
(70s-90s-slide #:conferences? #t)
(70s-90s-slide #:conference 'pldi)
(70s-90s-slide #:conference 'oopsla)
(70s-90s-slide #:conference 'popl)
(70s-90s-slide #:conference 'icfp)
(70s-90s-slide #:90s? #t)
(70s-90s-slide #:90s? #t #:types? #t)

(slide
 #:title "2000s"
 (table 2
        (list (para #:fill? #f (lt "Haskell") "," (lt "Scala"))
              (lt "JavaScript")
              (example haskell-example)
              (example javascript-example))
        ct-superimpose ct-superimpose
        (* 2 gap-size) gap-size)
 'next
 (item "Object-oriented" (it "and") "functional" (blank gap-size 0) "(not" (it "or") ")")
 (blank)
 'next
 (colorize (bit "Formal Methods") "forestgreen")
 (blank)
 'next
 (item "More types and even more static guarantess")
 'next
 (item "Untyped" (it "and") "typed"))

(define (topt s) (colorize (bt s) "forestgreen"))

(slide
 #:title "Some Current Topics"
 (vc-append
  (current-line-sep)
  (para (topt "Proofs and programs"))
  (item "About programs and about languages")
  (item "Program synthesis")) 
 (blank)
 (vc-append
  (current-line-sep)
  (para (topt "Compilers and Runtime systems"))
  (item "Optimization and JITs")
  (item "Memory management"))
 (blank)
 (vc-append
  (current-line-sep)
  (para (topt "Metaprogramming"))
  (item "Domain-specific languages (DSLs)")
  (item "Language workbenches")))

(slide
 #:title "2020s ???"
 #:layout 'top
 (blank)
 (lt "Racket")
 (example racket-example #:scale 0.6))

;; ----------------------------------------

(slide
 (titlet "Binding as Sets of Scopes")
 (bitmap (collection-file-path "PLT-206.png" "icons"))
 (ct "POPL 2016"))

#;
(slide
 (inset (colorize (filled-rectangle 1024 768) "black")
        (- margin)))

(scope-slides)
(terminology-slides)
