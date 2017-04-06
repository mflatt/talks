#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/nsstring
         racket/gui/base
         mred/private/wx/cocoa/types
         mred/private/wx/cocoa/utils
         racket/class)

(provide movie-panel%)

(void (ffi-lib "/System/Library/Frameworks/QTKit.framework/QTKit"))

(import-class QTMovie
              QTMovieView)

(define-cstruct _QTTime ([val _long]
                         [scale _long]
                         [flags _long]))

(define-objc-class MyMovieView QTMovieView
  []
  [-a _void (keyDown: evt)
      (when (equal? "a" (event-string evt))
        (tell self gotoBeginning: self))
      (super-tell keyDown: evt)])

(define (event-string evt)
  (tell #:type _NSString evt characters))

(define movie-panel%
  (class vertical-panel%
    (init file)

    (define sub-f (new frame%
                       [label "sub"]
                       [style '(float no-resize-border no-caption no-system-menu)]))

    (super-new)

    (inherit get-parent
             get-top-level-window
             get-width
             get-height
             get-x
             get-y)

    (define movie
      (as-objc-allocation-with-retain
       (tell QTMovie 
             movieWithFile: #:type _NSString (path->string 
                                              (path->complete-path file))
             error: #f)))

    (define movie-view
      (as-objc-allocation
       (tell (tell (or MyMovieView QTMovieView) alloc)
             initWithFrame: #:type _NSRect (make-NSRect (make-NSPoint 0 0)
                                                        (make-NSSize 800 600)))))

    (define/private (on-change x y w h)
      (define-values (fw fh) (send (get-top-level-window) get-client-size))
      (tellv movie-view
             setFrame: #:type _NSRect (make-NSRect (make-NSPoint x (- fh h y))
                                                   (make-NSSize w h)))
      (let ([f (get-top-level-window)])
        (send sub-f resize (- (send f get-width) (* 2 H-SPACE)) 30)
        (send sub-f move
              (+ (send f get-x) H-SPACE)
              (- (+ (send f get-y) (- (send f get-height) 35))
                 (- (send f get-height) (get-height))))))

    (define/override (on-size w h)
      (on-change (get-x) (get-y) w h))
    (define/override (on-move x y)
      (on-change x y (get-width) (get-height)))

    (tellv (send (get-parent) get-client-handle) addSubview: movie-view)

    (define msg "He is the most functional man in the world")
    (define/public (set-msg s) (set! msg s))
    
    (define c
      (new (class canvas% 
             (super-new)
             (inherit get-dc get-client-size)
             (define/override (on-paint)
               (define dc (get-dc))
               (define-values (w h) (get-client-size))
               (send dc set-text-foreground "white")
               (send dc set-font (make-font #:size 24))
               (define-values (tw th td ta) (send dc get-text-extent msg))
               (send dc draw-text msg (/ (- w tw) 2) 0)))
           [parent sub-f]))
    (send c set-canvas-background (make-color 0 0 0))

    (define ready? #f)

    (define/override (on-superwindow-show on?)
      (unless on?
        (set! ready? #f)
        (tellv movie-view pause: movie-view))
      (send sub-f show on?)
      (when on?
        (set! ready? #t)))

    (define H-SPACE 100)
    
    (define NSWindowAbove 1)
    (tellv (send (get-top-level-window) get-handle) addChildWindow: (send sub-f get-handle) ordered: #:type _int NSWindowAbove)

    (tellv movie-view setMovie: movie)
    (tellv movie-view play: movie-view)

    (define/public (set-current-time t)
      (define now (tell #:type _QTTime movie currentTime))
      (set-QTTime-val! now t)
      (tellv movie setCurrentTime: #:type _QTTime now))

    (define/public (get-current-time)
      (cond
       [ready?
        (define now (tell #:type _QTTime movie currentTime))
        (QTTime-val now)]
       [else 0]))))
