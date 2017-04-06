#lang slideshow
(require slideshow/code
         "person-icon.rkt"
         compatibility/defmacro)

(defmacro show-result (e)
  `(let ()
     (define as-text (code ,e))
     (define height (pict-height as-text))    
     (hc-append as-text
                (arrow height 0)
                ,e)))

(define height 100)
(show-result
 (person-icon height))


