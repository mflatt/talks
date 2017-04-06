#lang slideshow

(provide as-file)

(define (as-file s content)
  (let* ([content (inset (scale content 0.8) (/ gap-size 4))]
         [w (max (* 3 gap-size)
                 (+ 6 (pict-width content)))]
         [h (max (* 4 gap-size)
                 (+ gap-size (pict-height content)))])
    (let ([icon (file-icon w h "beige")])
      (let ([file (cc-superimpose
                   icon
                   content)])
        (if (not s)
            file
            (inset
             (vc-append
              (current-line-sep)
              file
              s)
             0 0 (/ (min 0 (- (pict-width icon) (pict-width s))) 2) 0))))))
