
(module start-a-lot mzscheme
  (require "spin-display.scm"
	   (lib "mred.ss" "mred"))

  (define (spin)
    (rotate-a-little)
    (sleep 0.1)
    (spin))
  (thread spin)

  (parameterize ((current-eventspace (make-eventspace)))
    (thread (lambda () (message-box "One" "Hi"))))
  (parameterize ((current-eventspace (make-eventspace)))
    (thread (lambda () (message-box "Two" "Bye")))))
