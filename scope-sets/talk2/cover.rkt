#lang racket/base
(require racket/class
         racket/draw
         pict3d
         pict3d-die-cut
         "cover-image.rkt")

(provide cover
         RR)

(define letters (combine (die-cut cover-image-path)
                         (pipe #:bottom-radii (interval 9/10 1)
                               #:top-radii (interval 9/10 1)
                               (pos (- RR)
                                    (- RR)
                                    0.0)
                               (pos RR
                                    RR
                                    -1.0))))
(define base (cylinder (pos (- RR)
                            (- RR)
                            -1.0)
                       (pos RR
                            RR
                            -3.0)))
(define cover
  (set-material
   (combine (set-color letters (apply rgba cover-letters-rgb))
            (set-color base (rgba cover-base-rgb)))
   (material #:ambient 0.3
             #:diffuse 0.2
             #:specular 0.5
             #:roughness 0.3)))
