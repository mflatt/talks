#lang racket/base
(require parser-tools/lex 
         (prefix-in : parser-tools/lex-sre)
         algol60/cfg-parser
         syntax/readerr
         (for-syntax racket/base))

(provide lex
         parse
         token->string)

(define (parse src-name in)
  (parameterize ([current-source src-name])
    (parse-from-lex (lambda () (lex in)))))

;; ----------------------------------------
;; Lexer

(define-tokens content-tokens
  (ID NUM))

(define-empty-tokens delim-tokens
  (THIS COPEN CCLOSE SOPEN SCLOSE 
        ASSIGN DOT PLUS SEMICOLON 
        WHITESPACE EOF ERROR))

(define lex
  (lexer-src-pos
   ["this" 'THIS]
   [(:+ (:/ #\A #\Z #\a #\z)) (token-ID (string->symbol lexeme))]
   [(:+ (:/ "0" "9")) (token-NUM (string->number lexeme))]
   ["{" 'COPEN]
   ["}" 'CCLOSE]
   ["[" 'SOPEN]
   ["]" 'SCLOSE]
   ["=" 'ASSIGN]
   ["." 'DOT]
   ["+" 'PLUS]
   [";" 'SEMICOLON]
   [(:+ whitespace) 'WHITESPACE]
   [(eof) 'EOF]
   [any-char 'ERROR]))

;; ----------------------------------------
;; Parser (using lexer's result tokens):

(define parse-from-lex
  (cfg-parser
   (start <prog>)
   (end EOF)
   (tokens content-tokens
           delim-tokens)
   (precs (left DOT ASSIGN))
   (error (lambda (a t v start end) 
            (raise-parse-error t v start end)))
   (src-pos)
   (grammar
    (<prog> [(<ws> <obj> <ws>) $2])
    (<obj> [(COPEN <members> <ws> CCLOSE) 
            (at-src `(obj . ,$2))])
    (<members> [() #'()]
               [(<ws> ID <ws> ASSIGN <ws> <rhs> <members>) 
                (cons (list $2 $6) $7)])
    (<rhs> [(SOPEN <stmts> <ws> <expr> <ws> SCLOSE) 
            (at-src `(block ,@$2 ,$4))]
           [(<expr> <ws> SEMICOLON) $1])
    (<stmts> [() '()]
             [(<ws> <stmt> <ws> SEMICOLON <stmts>) (cons $2 $5)])
    (<stmt> [(<expr> <ws> DOT <ws> ID <ws> ASSIGN <ws> <expr>) 
             (at-src `(assign ,$1 ,$5 ,$9))])
    (<expr> [(<expr> <ws> PLUS <ws> <oexpr>) 
             (at-src `(+ ,$1 ,$5))]
            [(<oexpr>) $1])
    (<oexpr> [(THIS) (at-src 'this)]
             [(NUM) (at-src $1)]
             [(<oexpr> <ws> DOT <ws> ID) (at-src `(select ,$1 ,$5))]
             [(<obj>) $1])
    (<ws> [() #f]
          [(WHITESPACE) #f]))))

(define-syntax (at-src stx)
  (syntax-case stx ()
    [(_ e) 
     (with-syntax ([start (datum->syntax stx '$1-start-pos)]
                   [end (datum->syntax stx '$n-end-pos)])
       #'(datum->syntax #f e (to-srcloc start end)))]))

;; ----------------------------------------
;; Source locations and error reporting:

(define current-source (make-parameter #f))

(define (to-srcloc start end)
  (list
   (current-source)
   (position-line start)
   (position-col start)
   (position-offset start)
   (and (position-offset end)
        (position-offset start)
        (- (position-offset end)
           (position-offset start)))))

(define (raise-parse-error t v start end)
  (apply
   (if (eq? t 'EOF) raise-read-eof-error raise-read-error) 
   (format "bad syntax at ~a" (token->string t v))
   (to-srcloc start end)))

(define (token->string t v)
  (case t
    [(COPEN) "{"]
    [(CCLOSE) "}"]
    [(SOPEN) "["]
    [(SCLOSE) "]"]
    [(ASSIGN) "="]
    [(DOT) "."]
    [(PLUS) "+"]
    [(SEMICOLON) ";"]
    [(WHITESPACE) "whitespace"]
    [(EOF) "end of file"]
    [else (format "~a" v)]))
