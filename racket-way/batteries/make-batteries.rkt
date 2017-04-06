#lang slideshow
(require racket/draw
         racket/class
         "../st.rkt")

(define battery (bitmap "battery.png"))
(define counter 0)

(define (make-battery p)
  (define (to-size s)
    (inexact->exact (ceiling s)))

  (define w (pict-width p))
  (define h (pict-height p))
  (define bm (make-bitmap (+ (to-size w) 2) (to-size h)))
  (define dc (make-object bitmap-dc% bm))
  (send dc set-smoothing 'smoothed)
  (draw-pict p dc 0 0)
  
  (define bw (pict-width battery))
  (define bh (pict-height battery))
  (define bbm (make-bitmap (to-size bw) (to-size bh)))
  (define bdc (make-object bitmap-dc% bbm))
  (send bdc set-smoothing 'smoothed)
  (draw-pict battery bdc 0 0)

  (when #f
    (define mp (floor (/ w 2)))
    (define hbw (quotient bw 2))
    (for ([i (in-range (- hbw 20) hbw)])
      (define delta (floor (* 20 (- 1 (cos (* pi 1/2 (/ (- i (- hbw 20)) 20)))))))
      (send dc copy (+ mp i) 0 1 h w 0)
      (send dc copy w 0 1 h (+ mp i) (- delta))
      (send dc copy (- mp i) 0 1 h w 0)
      (send dc copy w 0 1 h (- mp i) (- delta))))

  (send bdc draw-bitmap bm (/ (- bw w) 2) (* (- bh h) 2/3))
  
  (send bbm save-file (format "battery~a.png" counter) 'png)
  (set! counter (add1 counter)))

(for-each
 make-battery
 (list (scale (bitmap "opengl.png") 0.3)
       (cb-superimpose (scale (bitmap "plot.png") 0.3)
                       (st "plot"))
       (vc-append 5
                  (bitmap "button.png")
                  (st "GUI"))
       (rotate
        (inset (scale (st "<xml/>" #:t tt) 0.9) 0 0 0 10)
        (/ pi 2))
       (cc-superimpose (scale (bitmap "json160.gif") 0.5)
                       (st "JSON"))
       (cc-superimpose (scale (bitmap "web-server.png") 0.7)
                       (vc-append -10 
                                  (st "web")
                                  (st "server")))
       (cb-superimpose (colorize (text "c" '(bold . swiss) 96) "DeepSkyBlue")
                       (st "FFI"))
       (cc-superimpose (scale (bitmap "windows.png") 0.3)
                       (st "COM"))
       (cc-superimpose (cellophane (scale (bitmap "zip.png") 0.4) 0.8)
                       (rotate (scale (vc-append (- 10) (st "packaging") (st "tools")) 0.75)
                               (/ pi 2)))))

