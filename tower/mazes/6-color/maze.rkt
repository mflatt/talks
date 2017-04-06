#lang racket

(provide define-verbs
         define-thing
         define-place
         define-actions
         
         show-current-place
         show-inventory
         save-game
         load-game
         show-help

         have-item?
         take-item!
         drop-item!
         thing-state
         set-thing-state!

         (rename-out [module-begin #%module-begin])
         (except-out (all-from-out racket) #%module-begin))

;; ============================================================
;; Model:

;; The basic elements of the world:
(struct verb (aliases desc transitive?))
(struct thing (name [state #:mutable] actions))
(struct place (desc [things #:mutable] actions))

;; Tables mapping names<->things for save and load
(define names (make-hash))
(define things (make-hash))
(define (name->thing name) (hash-ref names name #f))
(define (thing->name obj) (hash-ref things obj #f))
(define (record-thing! name val)
  (hash-set! names name val)
  (hash-set! things val name))

;; ============================================================
;; Simple type layer:

(begin-for-syntax 
 (struct typed (id type) 
   #:property prop:procedure (lambda (self stx) (typed-id self))
   #:omit-define-syntaxes))

(define-syntax (check-type stx)
  (syntax-case stx ()
    [(_ id kind)
     (let ([v (and (identifier? #'id)
                   (syntax-local-value #'id (lambda () #f)))])
       (unless (and (typed? v)
                    (eq? (syntax-e #'kind) (typed-type v)))
         (raise-syntax-error
          #f
          (format "not a defined ~a" (syntax-e #'kind))
          #'id))
       #'id)]))

;; ============================================================
;; Macros for conveniently constructing and registering things:

(define-syntax-rule (define-verbs all-id 
                      [id . spec] ...)
  (begin
    (define-one-verb id . spec) ...
    (define all-id (list id ...))))

(define-syntax define-one-verb
  (syntax-rules (_)
    [(define-one-verb id (alias ...) desc)
     (begin
       (define gen-id (verb (list 'id 'alias ...) desc #f))
       (define-syntax id (typed #'gen-id 'verb)))]
    [(define-one-verb id _ (alias ...) desc)
     (begin
       (define gen-id (verb (list 'id 'alias ...) desc #t))
       (define-syntax id (typed #'gen-id 'transitive-verb)))]
    [(define-one-verb id)
     (define-one-verb id () (symbol->string 'id))]
    [(define-one-verb id _)
     (define-one-verb id _ () (symbol->string 'id))]))

(define-syntax-rule (define-thing id 
                      [cmd response ...] ...)
  (begin
    (define gen-id 
      (thing 'id #f (actions transitive-verb [cmd response ...] ...)))
    (define-syntax id (typed #'gen-id 'thing))
    (record-thing! 'id id)))

(define-syntax-rule (actions verb [cmd response ...] ...)
  (list (cons (check-type cmd verb) (lambda () response ...)) ...))


(define-syntax-rule (define-place id 
                      desc 
                      (thng ...) 
                      ([cmd response ...] ...))
  (begin
    (define gen-id (place desc
                          (list (check-type thng thing) ...)
                          (actions verb [cmd response ...] ...)))
    (define-syntax id (typed #'gen-id 'place))
    (record-thing! 'id id)))


(define-syntax-rule (define-actions id ([cmd response ...] ...))
  (define id (actions verb [cmd response ...] ...)))

;; ============================================================
;; Overall module:

(define-syntax module-begin
  (syntax-rules (define-verbs define-actions)
    [(_ (define-verbs all-verbs cmd ...)
        (define-actions world-actions act ...)
        decl ...
        start)
     (#%module-begin
      (define-verbs all-verbs cmd ...)
      (define-actions world-actions act ...)
      decl ...
      (start-game start
                  all-verbs
                  world-actions))]))

;; ============================================================
;; Game state

(define stuff null)
(define current-place #f)

(define (have-item? i) (memq i stuff))
(define (take-item! i) 
  (set-place-things! current-place (remq i (place-things current-place)))
  (set! stuff (cons i stuff)))
(define (drop-item! i) 
  (set-place-things! current-place (cons i (place-things current-place)))
  (set! stuff (remq i stuff)))

;; ============================================================
;; Game execution

(define all-verbs #f)
(define world-actions #f)

(define (do-place)
  (show-current-place)
  (do-verb))

(define (show-current-place)
  (printf "~a\n" (place-desc current-place))
  (for-each (lambda (thing)
              (printf "There is a ~a here.\n" (thing-name thing)))
            (place-things current-place)))

(define (do-verb)
  (printf "> ")
  (flush-output)
  (let* ([line (read-line)]
         [input (if (eof-object? line)
                    '(quit)
                    (let ([port (open-input-string line)])
                      (for/list ([v (in-port read port)]) v)))])
    (if (and (list? input)
             (andmap symbol? input)
             (<= 1 (length input) 2))
        (let ([cmd (car input)])
          (let ([response
                 (cond
                  [(= 2 (length input))
                   (handle-thing-verb cmd (cadr input))]
                  [(= 1 (length input))
                   (handle-direct-verb cmd)])])
            (let ([result (response)])
              (cond
               [(place? result)
                (set! current-place result)
                (do-place)]
               [(string? result)
                (printf "~a\n" result)
                (do-verb)]
               [else (do-verb)]))))
        (begin
          (printf "I don't undertand what you mean.\n")
          (do-verb)))))

(define (handle-direct-verb cmd)
  (or
   (find-verb cmd (place-actions current-place))
   (find-verb cmd world-actions)
   (using-verb 
    cmd all-verbs
    (lambda (verb)
      (lambda () 
        (if (verb-transitive? verb)
            (format "~a what?" (string-titlecase (verb-desc verb)))
            (format "Can't ~a here." (verb-desc verb))))))
   (lambda ()
     (format "I don't know how to ~a." cmd))))

(define (handle-thing-verb cmd obj)
  (or (using-verb 
       cmd all-verbs
       (lambda (verb)
         (and 
          (verb-transitive? verb)
          (cond
           [(ormap (lambda (thing)
                     (and (eq? (thing-name thing) obj)
                          thing))
                   (append (place-things current-place)
                           stuff))
            => (lambda (thing)
                 (or (find-verb cmd (thing-actions thing))
                     (lambda ()
                       (format "Don't know how to ~a ~a."
                               (verb-desc verb) obj))))]
           [else
            (lambda ()
              (format "There's no ~a here to ~a." obj 
                      (verb-desc verb)))]))))
      (lambda ()
        (format "I don't know how to ~a ~a." cmd obj))))

(define (show-inventory)
  (printf "You have")
  (if (null? stuff)
      (printf " no items.")
      (for-each (lambda (thing)
                  (printf "\n  a ~a" (thing-name thing)))
                stuff))
  (printf "\n"))

(define (find-verb cmd actions)
  (ormap (lambda (a)
           (and (memq cmd (verb-aliases (car a)))
                (cdr a)))
         actions))

(define (using-verb cmd verbs success-k)
  (ormap (lambda (verb)
           (and (memq cmd (verb-aliases verb))
                (success-k verb)))
         verbs))

(define (show-help)
  (printf "Use `look' to look around.\n")
  (printf "Use `inventory' to see what you have.\n")
  (printf "Use `save' or `load' to save or restore your game.\n")
  (printf "There are some other verbs, and you can name an thing after some verbs.\n"))

;; ============================================================
;; Save and load

(define (with-filename proc)
  (printf "File name: ")
  (flush-output)
  (let ([v (read-line)])
    (unless (eof-object? v)
      (with-handlers ([exn? (lambda (exn)
                              (printf "~a\n" (exn-message exn)))])
        (unless (path-string? v)
          (raise-user-error "bad filename"))
        (proc v)))))

(define (save-game)
  (with-filename
   (lambda (v)
     (with-output-to-file v
       (lambda ()
         (write
          (list
           (map thing->name stuff)
           (thing->name current-place)
           (hash-map names
                     (lambda (k v)
                       (cons k
                             (cond
                              [(place? v) (map thing->name (place-things v))]
                              [(thing? v) (thing-state v)]
                              [else #f])))))))))))

(define (load-game)
  (with-filename
   (lambda (v)
     (let ([v (with-input-from-file v read)])
       (set! stuff (map name->thing (car v)))
       (set! current-place (name->thing (cadr v)))
       (for-each
        (lambda (p)
          (let ([v (name->thing (car p))]
                [state (cdr p)])
            (cond
             [(place? v) (set-place-things! v (map name->thing state))]
             [(thing? v) (set-thing-state! v state)])))
        (caddr v))))))

;; ============================================================
;; To go:

(define (start-game in-place
                    in-all-verbs
                    in-world-actions)
  (set! current-place in-place)
  (set! all-verbs in-all-verbs)
  (set! world-actions in-world-actions)
  (do-place))
