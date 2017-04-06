#lang racket

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
;; Macros for conveniently constructing and registering things:

(define-syntax-rule (define-verbs all-id
                      [id spec ...] ...)
  (begin
    (define id (one-verb id spec ...)) ...
    (record-thing! 'id id) ...
    (define all-id (list id ...))))

(define-syntax one-verb
  (syntax-rules (_)
    [(one-verb id (alias ...) desc)
     (verb (list 'id 'alias ...) desc #f)]
    [(one-verb id _ (alias ...) desc)
     (verb (list 'id 'alias ...) desc #t)]
    [(one-verb id)
     (verb (list 'id) (symbol->string 'id) #f)]
    [(one-verb id _)
     (verb (list 'id) (symbol->string 'id) #t)]))


(define-syntax-rule (define-thing id 
                      [cmd response ...] ...)
  (begin
    (define id (thing 'id #f (actions [cmd response ...] ...)))
    (record-thing! 'id id)))

(define-syntax-rule (actions [cmd response ...] ...)
  (list (cons cmd (lambda () response ...)) ...))


(define-syntax-rule (define-place id 
                      desc 
                      (thing ...) 
                      ([cmd response ...] ...))
  (begin
    (define id (place desc
                      (list thing ...)
                      (actions [cmd response ...] ...)))
    (record-thing! 'id id)))


(define-syntax-rule (define-actions id ([cmd response ...] ...))
  (define id (actions [cmd response ...] ...)))

;; ============================================================
;; The world:

;; Verbs ----------------------------------------
;; Declare all the verbs that can be used in the game.
;; Each verb has a canonical name, a `_' if it needs
;; a thing (i.e., a transitive verb), a set of aliases, 
;; and a printed form.

(define-verbs all-verbs
  [north (n) "go north"]
  [south (s) "go south"]
  [east (e) "go east"]
  [west (w) "go west"]
  [up () "go up"]
  [down () "go down"]
  [in (enter) "enter"]
  [out (leave) "leave"]  
  [get _ (grab take) "take"]
  [put _ (drop leave) "drop"]
  [open _ (unlock) "open"]
  [close _ (lock) "close"]
  [knock _]
  [quit (exit) "quit"]
  [look (show) "look"]
  [inventory () "check inventory"]
  [help]
  [save]
  [load])

;; Global actions ----------------------------------------
;; Handle verbs that work anywhere.

(define-actions everywhere-actions
  ([quit (printf "Bye!\n") (exit)]
   [look (show-current-place)]
   [inventory (show-inventory)]
   [save (save-game)]
   [load (load-game)]
   [help (show-help)]))

;; Things ----------------------------------------
;; Each thing handles a set of transitive verbs.

(define-thing cactus
  [get "Ouch!"])

(define-thing door
  [open (if (have-item? key)
            (begin
              (set-thing-state! door 'open)
              "The door is now unlocked and open.")
            "The door is locked.")]
  [close (set-thing-state! door #f)
         "The door is now closed."]
  [knock "No one is home."])

(define-thing key
  [get (if (have-item? key)
           "You already have the key."
           (begin
             (take-item! key)
             "You now have the key."))]
  [put (if (have-item? key)
           (begin
             (drop-item! key)
             "You have dropped the key.")
           "You don't have the key.")])

(define-thing trophy
  [get (begin
         (take-item! trophy)
         "You win!")])

;; Places ----------------------------------------
;; Each place handles a set of non-transitive verbs.

(define-place start
  "You're standing in a field. There is a house to the north."
  []
  ([north house-front]
   [south desert]))

(define-place house-front
  "You are standing in front of a house."
  [door]
  ([in (if (eq? (thing-state door) 'open)
           room
           "The door is not open.")]
   [south start]))

(define-place desert
  "You're in a desert. There is nothing for miles around."
  [cactus key]
  ([north start]
   [south desert]
   [east desert]
   [west desert]))

(define-place room
  "You're in the house."
  [trophy]
  ([out house-front]))

;; ============================================================
;; Game state

(define stuff null)
(define current-place start)

(define (have-item? i) (memq i stuff))
(define (take-item! i) 
  (set-place-things! current-place (remq i (place-things current-place)))
  (set! stuff (cons i stuff)))
(define (drop-item! i) 
  (set-place-things! current-place (cons i (place-things current-place)))
  (set! stuff (remq i stuff)))

;; ============================================================
;; Game execution

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
   (find-verb cmd everywhere-actions)
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
  (printf "There are some other verbs, and you can name a thing after some verbs.\n"))

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
;; Go!

(do-place)
