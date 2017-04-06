
(define q (queue))

(put q 'blue)

(put q 'red)

(get q)

----------------------------------------

(define (loop)
  (put q 'green)
  (get q)
  (loop))

(define t (spawn loop))

(thread-suspend t)
(thread-resume t)

----------------------------------------

(define c (make-custodian))

(parameterize ([current-custodian c])
  (spawn loop))

(custodian-shutdown-all c)

----------------------------------------

(define ch (channel))

(define (put-loop)
  (put q (channel-recv ch))
  (put-loop))
(spawn put-loop)

(channel-send ch 'red)

----------------------------------------

(define ch (channel))

(define (get-loop)
  (channel-send ch (get q))
  (get-loop))
(spawn get-loop)

(channel-recv ch)

----------------------------------------

(define get-ch (channel))
(define put-ch (channel))

(define (q-loop)
  ... (channel-send get-ch (get q))
  ... OR
  ... (put q (channel-recv put-ch))
  (q-loop))

----------------------------------------

(define (q-loop)
  (sync
   (choice-evt
    (channel-send-evt get-ch (get q))
    (channel-recv-evt put-ch)))
  (q-loop))

(define (channel-recv ch)
  (sync (channel-recv-evt ch)))

(define (channel-send ch v)
  (sync (channel-send-evt ch v)))

...

----------------------------------------

(define (q-loop)
  (sync
   (choice-evt
    (channel-send get-ch (get q))
    (wrap-evt (channel-recv put-ch)
	      (lambda (v)
		(put q v)))))
  (q-loop))

...

----------------------------------------

(define (q-loop)
  (sync
   (choice-evt
    (wrap-evt (channel-send get-ch (peek q))
	      (lambda () (get q)))
    (wrap-evt (channel-recv put-ch)
	      (lambda (v) (put q v)))))
  (q-loop))

(spawn q-loop)

(define (loop)
  (channel-send put-ch 'green)
  (channel-recv get-ch)
  (loop))

(parameterize ([current-custodian c])
  (spawn loop)
  (spawn loop))

(custodian-shutdown-all c)

----------------------------------------

(define-struct safe-q (put-ch get-ch))

(define (safe-queue)
  (define q (queue))
  (define get-ch (channel))
  (define put-ch (channel))
  (define (q-loop)
    (sync
     (choice-evt
      (wrap-evt 
       (channel-send get-ch (peek q))
       (lambda () (get q)))
      (wrap-evt 
       (channel-recv put-ch)
       (lambda (v) (put q v)))))
    (q-loop))

  (spawn q-loop)  
  (make-safe-q put-ch get-ch))

(define (safe-get-evt sq)
  (channel-recv-evt (safe-q-get-ch sq)))

(define (safe-put-evt sq v)
  (channel-send-evt (safe-q-put-ch sq) v))

(define (safe-get sq)
  (sync (safe-get-evt sq)))

(define (safe-put sq v)
  (sync (safe-put-evt sq v)))

============================================================

Picture of custodians: higher-level one runs the queue

What if we can't install a parent?


(define c1 (make-custodian))
(define c2 (make-custodian))

(parameterize ([current-custodian c1])
  (spawn (lambda ()
	    (define sq (safe-queue))
	    (register 'queue sq)
	    (safe-put sq 'blue))))

(parameterize ([current-custodian c2])
  (spawn (lambda ()
	   (define sq (lookup 'queue))
	   (define (loop)
	     (safe-put sq 'red)
	     (safe-get sq)
	     (loop)))))

(custodian-shutdown-all c1)

----------------------------------------

(define (safe-queue)
  ...
  (define manager-t (spawn q-loop))
  (make-safe-q manager-t put-ch get-ch))

(define (safe-get sq)
  (thread-resume (safe-q-manager-t sq))
  (sync (safe-get-evt sq)))

...

----------------------------------------

(define (safe-queue)
  ...
  (define manager-t (spawn q-loop))
  (make-safe-q manager-t put-ch get-ch))

(define (safe-get-evt sq)
  (guard-evt
   (lambda ()
     (thread-resume (safe-q-manager-t sq))
     (channel-recv-evt (safe-q-get-ch sq)))))

...

----------------------------------------

(define (safe-queue)
  ...
  (define manager-t (spawn q-loop))
  (make-safe-q manager-t put-ch get-ch))

(define (safe-get-evt sq)
  (guard-evt
   (lambda ()
     (thread-resume (safe-q-manager-t sq) (current-thread))
     (channel-recv-evt (safe-q-get-ch sq)))))

----------------------------------------

[Assemble and demo]

============================================================

Further elaboration:

 * Some operations require info in both directions. E.g.,
   a more specific request and the corresponding reply

    -> nack to detect giving up

 * 