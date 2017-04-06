  (define esq-eventspace (current-eventspace))
  (define (queue-output proc)
    (parameterize ((current-eventspace
                    esq-eventspace))
      (queue-callback proc #f)))

  (define esq-text%
    (class text%
      (inherit insert last-position get-text erase)                 
      (define prompt-pos 0) 
      (define locked? #t)
      (augment*
        (can-insert? (lambda (start len) (and (>= start prompt-pos) (not locked?))))
        (can-delete? (lambda (start end) (and (>= start prompt-pos) (not locked?)))))
      (override*
        (on-char (lambda (c)
                   (super on-char c)
                   (when (and (eq? (send c get-key-code) #\return) (not locked?))
                     (set! locked? #t)
                     (evaluate (get-text prompt-pos (last-position)))))))
      (public*
        (new-prompt (lambda () (queue-output
                                 (lambda ()
                                   (set! locked? #f)
                                   (insert "> ")
                                   (set! prompt-pos (last-position))))))
        (output (lambda (str) (queue-output
                               (lambda ()
                                 (let ((was-locked? locked?))
                                   (set! locked? #f)
                                   (insert str)
                                   (set! locked? was-locked?))))))
        (reset (lambda ()
                 (set! locked? #f)
                 (set! prompt-pos 0)
                 (erase)
                 (new-prompt))))
      (super-instantiate ())

      (inherit get-style-list)
      (let ([s (send (get-style-list) find-named-style "Standard")])
	(when s
	  (let ([d (make-object style-delta% 'change-size 24)])
	    (send s set-delta d))))

      (new-prompt)))



