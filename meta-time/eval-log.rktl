;; @ Wednesday, April 9th, 2014 10:00:10am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Wednesday, April 9th, 2014 10:01:13am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Sunday, May 18th, 2014 10:38:32am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Sunday, May 18th, 2014 8:36:35pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Sunday, May 18th, 2014 8:36:52pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Sunday, May 18th, 2014 8:37:21pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
(sleep 1)
now
;; @ Sunday, May 18th, 2014 8:38:20pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Sunday, May 18th, 2014 8:38:25pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
(sleep 1)
now
;; @ Sunday, May 18th, 2014 8:39:16pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/today.rkt
#lang racket

now
;; @ Sunday, May 18th, 2014 8:39:25pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

(provide now)
;; @ Sunday, May 18th, 2014 8:39:32pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/today.rkt
#lang racket
(require "now.rkt")

now
;; @ Sunday, May 18th, 2014 8:39:32pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

(provide now)
;; @ Sunday, May 18th, 2014 8:41:11pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/pause.rkt
#lang racket
(require "past.rkt")

past
(sleep 1)
past
;; @ Sunday, May 18th, 2014 8:41:11pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/past.rkt
#lang racket

(define now
  (current-seconds))
 
(define-syntax (past stx)
  #'(- (current-seconds)
       now))

(provide past)
;; @ Sunday, May 18th, 2014 8:41:41pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
(module racket clock

  "tick")
;; @ Sunday, May 18th, 2014 8:41:46pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
(module clock racket

  "tick")
;; @ Sunday, May 18th, 2014 8:41:53pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

"tick"

;; @ Sunday, May 18th, 2014 8:42:10pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

"tick"

(module test racket
  "tock")

;; @ Sunday, May 18th, 2014 8:42:12pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

"tick"

(module test racket
  "tock")

;; @ Monday, May 19th, 2014 6:15:49am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

"tick"

(module racket test
  "tock")

;; @ Monday, May 19th, 2014 6:15:55am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

"tick"

(module test racket
  "tock")

;; @ Monday, May 19th, 2014 6:15:57am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

"tick"

(module test racket
  "tock")

;; @ Monday, May 19th, 2014 6:16:04am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module test racket
  "tock")

;; @ Monday, May 19th, 2014 6:16:09am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module test racket
  tick)

;; @ Monday, May 19th, 2014 6:16:30am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module* test #f
  tick)

;; @ Monday, May 19th, 2014 6:16:32am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module* test #f
  tick)

;; @ Monday, May 19th, 2014 6:17:05am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module+ test
  tick)

;; @ Monday, May 19th, 2014 6:17:24am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module+ test
  (identifier-binding #'tick))

;; @ Monday, May 19th, 2014 6:17:43am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(begin-for-syntax
 (module+ test
   (identifier-binding #'tick)))


;; @ Monday, May 19th, 2014 6:17:56am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(begin-for-syntax
 (module+ test
   (identifier-template-binding #'tick)))


;; @ Monday, May 19th, 2014 11:16:10am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Monday, May 19th, 2014 11:26:08am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Monday, May 19th, 2014 11:26:17am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Monday, May 19th, 2014 11:26:23am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Monday, May 19th, 2014 11:34:56am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Monday, May 19th, 2014 11:35:02am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Monday, May 19th, 2014 11:35:05am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Monday, May 19th, 2014 11:35:13am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

(- now)
;; @ Monday, May 19th, 2014 11:35:24am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/today.rkt
#lang racket

now

;; @ Monday, May 19th, 2014 11:35:35am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/today.rkt
#lang racket
(require "now.rkt")
now

;; @ Monday, May 19th, 2014 11:35:35am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(provide now)

(define now
  (current-seconds))

(- now)
;; @ Monday, May 19th, 2014 11:35:58am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/pause.rkt
#lang racket
(require "past.rkt")

past
(sleep 1)
past
;; @ Monday, May 19th, 2014 11:35:58am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/past.rkt
#lang racket

(define now
  (current-seconds))
 
(define-syntax (past stx)
  #'(- (current-seconds)
       now))

(provide past)
;; @ Monday, May 19th, 2014 11:36:57am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/pause.rkt
#lang racket
(require "past.rkt")

past
(sleep 1)
past
now

;; @ Monday, May 19th, 2014 11:36:57am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/past.rkt
#lang racket

(define now
  (current-seconds))
 
(define-syntax (past stx)
  #'(- (current-seconds)
       now))

(provide past)
;; @ Monday, May 19th, 2014 11:37:27am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

"tick"

(module test racket
  "tock")

;; @ Monday, May 19th, 2014 11:37:28am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

"tick"

(module test racket
  "tock")

;; @ Monday, May 19th, 2014 11:37:36am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module test racket
  "tock")

;; @ Monday, May 19th, 2014 11:37:40am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module test racket
  tick)

;; @ Monday, May 19th, 2014 11:37:51am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module* test #f
  tick)

;; @ Monday, May 19th, 2014 11:37:56am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module* test #f
  tick)

;; @ Monday, May 19th, 2014 11:38:07am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module* test #f
  #'tick)

;; @ Monday, May 19th, 2014 11:38:08am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module* test #f
  #'tick)

;; @ Monday, May 19th, 2014 11:38:16am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module* test #f
  (identifier-binding #'tick))

;; @ Monday, May 19th, 2014 11:38:24am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(begin-for-syntax
  (module* test #f
    (identifier-binding #'tick)))

;; @ Monday, May 19th, 2014 11:38:47am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/past.rkt
#lang racket

(define now
  (current-seconds))

(define (gen)
  #'(- (current-seconds)
       now))

(define-syntax (past stx)
  (gen))
  
(provide past)
;; @ Monday, May 19th, 2014 11:38:54am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/past.rkt
#lang racket

(define now
  (current-seconds))

(begin-for-syntax
  (define (gen)
    #'(- (current-seconds)
         now)))

(define-syntax (past stx)
  (gen))
  
(provide past)
;; @ Monday, May 19th, 2014 11:38:58am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/pause.rkt
#lang racket
(require "past.rkt")

past
(sleep 1)
past

;; @ Monday, May 19th, 2014 11:38:58am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/past.rkt
#lang racket

(define now
  (current-seconds))

(begin-for-syntax
  (define (gen)
    #'(- (current-seconds)
         now)))

(define-syntax (past stx)
  (gen))
  
(provide past)
;; @ Monday, May 19th, 2014 12:21:57pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Monday, May 19th, 2014 12:22:09pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

(- now)
;; @ Monday, May 19th, 2014 12:22:23pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/today.rkt
#lang racket

now

;; @ Monday, May 19th, 2014 12:22:43pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/today.rkt
#lang racket

(require "now.rkt")

now

;; @ Monday, May 19th, 2014 12:22:43pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(provide now)

(define now
  (current-seconds))

(- now)
;; @ Monday, May 19th, 2014 12:23:38pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/pause.rkt
#lang racket
(require "past.rkt")

past
(sleep 1)
past
;; @ Monday, May 19th, 2014 12:23:38pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/past.rkt
#lang racket

(define now
  (current-seconds))
 
(define-syntax (past stx)
  #'(- (current-seconds)
       now))

(provide past)
;; @ Monday, May 19th, 2014 12:24:51pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/pause.rkt
#lang racket
(require "past.rkt")

past
(sleep 1)
(past 17 hello bye)
;; @ Monday, May 19th, 2014 12:24:51pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/past.rkt
#lang racket

(define now
  (current-seconds))
 
(define-syntax (past stx)
  #'(- (current-seconds)
       now))

(provide past)
;; @ Monday, May 19th, 2014 12:25:43pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/past.rkt
#lang racket

(define now
  (current-seconds))
 
(define (gen)
  #'(- (current-seconds)
       now))

(define-syntax (past stx)
  (gen))
  
(provide past)
;; @ Monday, May 19th, 2014 12:26:23pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/pause.rkt
#lang racket
(require "past.rkt")

past
(sleep 1)
past

;; @ Monday, May 19th, 2014 12:26:23pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/past.rkt
#lang racket

(define now
  (current-seconds))
 
(begin-for-syntax
  (define (gen)
    #'(- (current-seconds)
         now)))

(define-syntax (past stx)
  (gen))
  
(provide past)
;; @ Monday, May 19th, 2014 12:28:09pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

"tick"
;; @ Monday, May 19th, 2014 12:28:16pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

"tick"

(module test racket
  "tock")

;; @ Monday, May 19th, 2014 12:28:21pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

"tick"

(module test racket
  "tock")

;; @ Monday, May 19th, 2014 12:28:41pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module test racket
  tick)

;; @ Monday, May 19th, 2014 12:29:05pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module* test #f
  tick)

;; @ Monday, May 19th, 2014 12:29:09pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(module* test #f
  tick)

;; @ Monday, May 19th, 2014 12:29:51pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(begin-for-syntax
  (module* test #f
    #'tick))

;; @ Monday, May 19th, 2014 12:29:56pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(begin-for-syntax
  (module* test #f
    #'tick))

;; @ Monday, May 19th, 2014 12:30:20pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(begin-for-syntax
  (module* test #f
    (identifier-template-binding #'tick)))

;; @ Monday, May 19th, 2014 12:30:24pm -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/clock.rkt
#lang racket

(define tick "tick")
tick

(begin-for-syntax
  (module* test #f
    (identifier-template-binding #'tick)))

;; @ Thursday, January 14th, 2016 5:46:50am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Thursday, January 14th, 2016 5:48:30am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Monday, March 13th, 2017 9:43:37am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Monday, March 13th, 2017 10:25:38am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Thursday, March 16th, 2017 7:11:47am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Thursday, March 16th, 2017 7:12:23am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Thursday, March 16th, 2017 10:18:47am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Thursday, March 16th, 2017 10:18:48am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Thursday, March 16th, 2017 10:19:00am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Thursday, March 16th, 2017 10:19:07am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Thursday, March 23rd, 2017 7:42:54am -----------
;; /Users/mflatt/svn/mflatt/text/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
;; @ Thursday, April 6th, 2017 1:57:39pm -----------
;; /Users/mflatt/repo/mflatt/talks/meta-time/now.rkt
#lang racket

(define now
  (current-seconds))

now
