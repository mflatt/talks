#lang at-exp racket/base
(require "doc-define.rkt"
         racket/contract/base
         (for-doc racket/base
                  scribble/manual))

(define (seconds->hours secs)
  #:contract (exact-integer? . -> . exact-integer?)
  #:doc @{Takes @code{secs}, a number of seconds
                since the epoch, and converts it to a number
                of days since the epoch, rounding down.
                
                For example, compose with with the
                @code{current-seconds} function to get
                @code{current-hours}.}
  (quotient secs (* 60 60)))

(begin-for-doc
 (define (current-docs what)
   @list{Returns the current @what since the epoch.}))

(define (current-seconds)
  #:contract (-> exact-integer?)
  #:doc @{@current-docs["seconds"]}
  (inexact->exact 
   (floor (/ (current-inexact-milliseconds) 1000))))

(define (current-hours)
  #:contract (-> exact-integer?)
  #:doc @{@current-docs["hours"]}
  (seconds->hours (current-seconds)))
