;; @ > Module
;; @ Wednesday, March 20th, 2013 2:57:55pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Wednesday, March 20th, 2013 2:58:04pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
(sleep 1)
now
;; @ > Modules
;; @ Wednesday, March 20th, 2013 2:58:37pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/today.rkt
#lang racket
(require "now.rkt")

now

;; @ Wednesday, March 20th, 2013 2:58:37pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Wednesday, March 20th, 2013 2:58:47pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

(provide now)
;; @ Wednesday, March 20th, 2013 2:58:51pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/today.rkt
#lang racket
(require "now.rkt")

now

;; @ Wednesday, March 20th, 2013 2:58:51pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

(provide now)
;; @ Wednesday, March 20th, 2013 2:59:06pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket
(require "today.rkt")
(define now
  (current-seconds))

(provide now)
;; @ Wednesday, March 20th, 2013 2:59:06pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/today.rkt
#lang racket
(require "now.rkt")

now

;; @ Wednesday, March 20th, 2013 2:59:06pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket
(require "today.rkt")
(define now
  (current-seconds))

(provide now)
;; @ > The module Form
;; @ Wednesday, March 20th, 2013 3:00:02pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
(module clock.rkt racket

  "tick")
;; @ Wednesday, March 20th, 2013 3:00:18pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
(module clock.rkt racket

  (module tick racket
    "tick")
  
  )
;; @ Wednesday, March 20th, 2013 3:00:39pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
(module clock.rkt racket

  (module tick racket
    "tick")
  
  (require (submod "." tick))
  )
;; @ Wednesday, March 20th, 2013 3:00:50pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
(module clock.rkt racket

  (module tick racket
    "tick")
  (module tock racket
    "tock")
  
  (require (submod "." tick))
  )
;; @ Wednesday, March 20th, 2013 3:00:54pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
(module clock.rkt racket

  (module tick racket
    "tick")
  (module tock racket
    "tock")
  
  (require (submod "." tock))
  )
;; @ Wednesday, March 20th, 2013 3:01:03pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
(module clock.rkt racket

  (module tick racket
    "tick")
  (module tock racket
    (require (submod ".." tick))
    "tock")
  
  (require (submod "." tock))
  )
;; @ Wednesday, March 20th, 2013 3:01:37pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
(module clock.rkt racket

  (define sound "tick")
  
  (module tick racket
    sound)
  
  (require (submod "." tick))
  )
;; @ Wednesday, March 20th, 2013 3:01:54pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
(module clock.rkt racket

  (define sound "tick")
  
  (module* tick #f
    sound)
  )
;; @ Wednesday, March 20th, 2013 3:02:13pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
(module clock.rkt racket

  (define sound "tick")
  
  (module* test #f
    sound)
  )
;; @ ! test
;; @ Wednesday, March 20th, 2013 3:02:14pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
(module clock.rkt racket

  (define sound "tick")
  
  (module* test #f
    sound)
  )
;; @ > Symbols and Syntax Objects
;; @ Wednesday, March 20th, 2013 3:04:01pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tick.rkt
#lang racket

(define sound "tick")

(define id 'sound)
(provide id)
sound
id
;; @ Wednesday, March 20th, 2013 3:04:17pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tock.rkt
#lang racket

(define sound "tock")

(require "tick.rkt")
sound
id
;; @ Wednesday, March 20th, 2013 3:04:17pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tick.rkt
#lang racket

(define sound "tick")

(define id 'sound)
(provide id)

;; @ Wednesday, March 20th, 2013 3:04:26pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tock.rkt
#lang racket

(define sound "tock")

(require "tick.rkt")
'sound
id
;; @ Wednesday, March 20th, 2013 3:04:26pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tick.rkt
#lang racket

(define sound "tick")

(define id 'sound)
(provide id)

;; @ Wednesday, March 20th, 2013 3:04:46pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tock.rkt
#lang racket

(define sound "tock")

(require "tick.rkt")
(eval 'sound)
;; @ Wednesday, March 20th, 2013 3:04:46pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tick.rkt
#lang racket

(define sound "tick")

(define id 'sound)
(provide id)

;; @ Wednesday, March 20th, 2013 3:05:02pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tock.rkt
#lang racket

(define sound "tock")

(require "tick.rkt")
(eval #'sound)
;; @ Wednesday, March 20th, 2013 3:05:02pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tick.rkt
#lang racket

(define sound "tick")

(define id 'sound)
(provide id)

;; @ Wednesday, March 20th, 2013 3:05:19pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tock.rkt
#lang racket

(define sound "tock")

(require "tick.rkt")
(eval id)
;; @ Wednesday, March 20th, 2013 3:05:19pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tick.rkt
#lang racket

(define sound "tick")

(define id #'sound)
(provide id)

;; @ Wednesday, March 20th, 2013 3:05:31pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tock.rkt
#lang racket

(define sound "tock")

(require "tick.rkt")
#'sound
id
;; @ Wednesday, March 20th, 2013 3:05:31pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tick.rkt
#lang racket

(define sound "tick")

(define id #'sound)
(provide id)

;; @ Wednesday, March 20th, 2013 3:05:57pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tock.rkt
#lang racket

(define sound "tock")

(require "tick.rkt")
(eval #'(list sound
              "!"))
;; @ Wednesday, March 20th, 2013 3:05:57pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tick.rkt
#lang racket

(define sound "tick")

(define id #'sound)
(provide id)

;; @ Wednesday, March 20th, 2013 3:06:07pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tock.rkt
#lang racket

(define sound "tock")

(require "tick.rkt")
(eval #`(list #,id
              "!"))
;; @ Wednesday, March 20th, 2013 3:06:07pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/tick.rkt
#lang racket

(define sound "tick")

(define id #'sound)
(provide id)

;; @ > Macros
;; @ Wednesday, March 20th, 2013 3:06:36pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
(sleep 1)
now
;; @ Wednesday, March 20th, 2013 3:07:09pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define-syntax now
  (lambda (stx)
    #'(current-seconds)))

now
(sleep 1)
now
;; @ Wednesday, March 20th, 2013 3:07:25pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define-syntax now
  (lambda (stx)
    #'(current-seconds)))

now

;; @ Wednesday, March 20th, 2013 3:07:40pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define-syntax now
  (lambda (stx)
    #'(current-seconds)))

(now 1 2 3 whatever)


;; @ Wednesday, March 20th, 2013 3:07:48pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define-syntax now
  (lambda (stx)
    #'(current-seconds)))

(now 1 2 3 whatever)
now


;; @ Wednesday, March 20th, 2013 3:08:02pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define-syntax now
  (lambda (stx)
    #'(current-seconds)))

#'(now 1 2 3 whatever)
#'now


;; @ Wednesday, March 20th, 2013 3:08:16pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define-syntax now
  (lambda (stx)
    #'(current-seconds)))

(syntax-e #'now)

(syntax-e #'(now 1 2 3 whatever))


;; @ Wednesday, March 20th, 2013 3:09:01pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define-syntax now
  (lambda (stx)
    (if (symbol? (syntax-e stx))        
        #'(current-seconds)
        (raise-syntax-error
         #f
         "bad syntax"
         stx))))

now
(now 1 2 3 something)
;; @ Wednesday, March 20th, 2013 3:09:27pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define-syntax now
  (lambda (stx)
    #'(current-seconds)))
 
now
;; @ Wednesday, March 20th, 2013 3:09:44pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define-syntax (now stx)
  #'(current-seconds))
 
now
;; @ > Compile-Time Expressions
;; @ Wednesday, March 20th, 2013 3:10:12pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/then.rkt
#lang racket

(define-syntax (then stx)
  (current-seconds))

then
;; @ Wednesday, March 20th, 2013 3:10:26pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/then.rkt
#lang racket

(define-syntax (then stx)
  #`#,(current-seconds))

then
;; @ Wednesday, March 20th, 2013 3:10:48pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/then.rkt
#lang racket

(define-syntax (about-then stx)
  #`(- #,(current-seconds)
       10))

about-then
;; @ > Compile-Time Imports
;; @ Wednesday, March 20th, 2013 3:11:12pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/then.rkt
#lang racket/base

(define-syntax (about-then stx)
  #`(- #,(current-seconds)
       10))

about-then
;; @ Wednesday, March 20th, 2013 3:11:37pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/then.rkt
#lang racket/base
(require (for-syntax racket/base))

(define-syntax (about-then stx)
  #`(- #,(current-seconds)
       10))

about-then
;; @ Wednesday, March 20th, 2013 3:12:04pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/about-then.rkt
#lang racket/base
(require (for-syntax racket/base
                     "recent.rkt"))

(define-syntax (about-then stx)
  #`#,(recent-seconds))

about-then
;; @ Wednesday, March 20th, 2013 3:12:04pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/recent.rkt
#lang racket/base

(define (recent-seconds)
  (- (current-seconds) 10))

(provide recent-seconds)
;; @ Wednesday, March 20th, 2013 3:12:22pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/about-then.rkt
#lang racket/base
(require (for-syntax racket/base)
         "recent.rkt")

(define-syntax (about-then stx)
  #`#,(recent-seconds))

about-then
;; @ Wednesday, March 20th, 2013 3:12:22pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/recent.rkt
#lang racket/base

(define (recent-seconds)
  (- (current-seconds) 10))

(provide recent-seconds)
;; @ > Macro-Generating Macros
;; @ Wednesday, March 20th, 2013 3:13:24pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/main.rkt
#lang racket

(define-syntax (define-now-alias stx)
  (define id (cadr (syntax-e stx)))
  #`(define-syntax (#,id stx)
      #'(current-seconds)))

(define-now-alias now)
(define-now-alias at-this-moment)

at-this-moment
;; @ > Macro-Generating Macros and Imports
;; @ Wednesday, March 20th, 2013 3:13:52pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/main.rkt
#lang racket
(require "recent.rkt")

(define-syntax (define-now-alias stx)
  (define id (cadr (syntax-e stx)))
  #`(define-syntax (#,id stx)
      #'(recent-seconds)))

(define-now-alias now)
(define-now-alias at-this-moment)

at-this-moment
;; @ Wednesday, March 20th, 2013 3:13:52pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/recent.rkt
#lang racket/base

(define (recent-seconds)
  (- (current-seconds) 10))

(provide recent-seconds)
;; @ > Everything-Generating Macros
;; @ Wednesday, March 20th, 2013 3:15:08pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/main.rkt
#lang racket
(require "times.rkt")
(define-times recently about-then
  (lambda (t) (- t 10)))
recently
;; @ Wednesday, March 20th, 2013 3:15:08pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/times.rkt
#lang racket

(define-syntax (define-times stx)
  (define rt-id (cadr (syntax-e stx)))
  (define ct-id (caddr (syntax-e stx)))
  (define proc (cadddr (syntax-e stx)))
  #`(begin
      (define-syntax (#,rt-id stx)
        #'(#,proc (current-seconds)))
      (define-syntax (#,ct-id stx)
        #`#,(#,proc (current-seconds)))
      (provide #,rt-id #,ct-id)
      (module+ test
        (require rackunit)
        (check-equal? #,rt-id (begin (sleep 1) (- #,rt-id 1)))
        (check-equal? #,ct-id (begin (sleep 1) #,ct-id)))))

(provide define-times)
;; @ Wednesday, March 20th, 2013 3:15:39pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/main.rkt
#lang racket
(require "times.rkt")
(define-times recently about-then
  (lambda (t) (- t 10)))
recently
;; @ ! test
;; @ Wednesday, March 20th, 2013 3:15:39pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/times.rkt
#lang racket

(define-syntax (define-times stx)
  (define rt-id (cadr (syntax-e stx)))
  (define ct-id (caddr (syntax-e stx)))
  (define proc (cadddr (syntax-e stx)))
  #`(begin
      (define-syntax (#,rt-id stx)
        #'(#,proc (current-seconds)))
      (define-syntax (#,ct-id stx)
        #`#,(#,proc (current-seconds)))
      (provide #,rt-id #,ct-id)
      (module+ test
        (require rackunit)
        (check-equal? #,rt-id (begin (sleep 1) (- #,rt-id 2)))
        (check-equal? #,ct-id (begin (sleep 1) #,ct-id)))))

(provide define-times)
;; @ > Bindings and Phases
;; @ Wednesday, March 20th, 2013 3:16:19pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/main.rkt
#lang racket
(require "recent.rkt")

(define id #'recent-seconds)

(identifier-binding id)
(identifier-transformer-binding id)
;; @ Wednesday, March 20th, 2013 3:16:19pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/recent.rkt
#lang racket/base

(define (recent-seconds)
  (- (current-seconds) 10))

(provide recent-seconds)
;; @ Wednesday, March 20th, 2013 3:16:33pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/main.rkt
#lang racket
(require (for-syntax "recent.rkt"))

(define id #'recent-seconds)

(identifier-binding id)
(identifier-transformer-binding id)
;; @ Wednesday, March 20th, 2013 3:16:33pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/recent.rkt
#lang racket/base

(define (recent-seconds)
  (- (current-seconds) 10))

(provide recent-seconds)
;; @ Wednesday, March 20th, 2013 3:16:41pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/main.rkt
#lang racket
(require "recent.rkt"
         (for-syntax "recent.rkt"))

(define id #'recent-seconds)

(identifier-binding id)
(identifier-transformer-binding id)
;; @ Wednesday, March 20th, 2013 3:16:41pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/recent.rkt
#lang racket/base

(define (recent-seconds)
  (- (current-seconds) 10))

(provide recent-seconds)
;; @ Wednesday, March 20th, 2013 3:17:00pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/main.rkt
#lang racket
(require (rename-in racket/base
                    [current-seconds
                     recent-seconds])
         (for-syntax "recent.rkt"))

(define id #'recent-seconds)

(identifier-binding id)
(identifier-transformer-binding id)
;; @ Wednesday, March 20th, 2013 3:17:00pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/recent.rkt
#lang racket/base

(define (recent-seconds)
  (- (current-seconds) 10))

(provide recent-seconds)
