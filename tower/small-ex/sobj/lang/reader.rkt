#lang racket
(require "parser.rkt"
         parser-tools/lex)

(provide read-syntax
         get-info
         color-lexer)

;; To read a module:
(define (read-syntax src-name in)
  (port-count-lines! in)
  (define stx (parse src-name in))
  (datum->syntax #f `(module prog sobj ,stx)))

;; To get info about the language's environment support:
(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer) color-lexer]
      [else default])))

;; Environment support for token coloring:
(define (color-lexer in offset mode)
  ;; Get next token:
  (define tok (lex in))
  ;; Package classification with srcloc:
  (define (ret mode paren)
    (values (token->string (position-token-token tok)
                           (token-value (position-token-token tok)))
            mode 
            paren
            (position-offset (position-token-start-pos tok))
            (position-offset (position-token-end-pos tok))
            0 
            #f))
  ;; Convert token to classification:
  (case (token-name (position-token-token tok))
    [(EOF) (ret 'eof #f)]
    [(COPEN) (ret 'parenthesis '|{|)]
    [(CCLOSE) (ret 'parenthesis '|}|)]
    [(SOPEN) (ret 'parenthesis '|[|)]
    [(SCLOSE) (ret 'parenthesis '|]|)]
    [(NUM) (ret 'literal #f)]
    [(WHITESPACE) (ret 'white-space #f)]
    [(DOT ASSIGN PLUS SEMICOLON) (ret 'other #f)]
    [(ERROR) (ret 'error #f)]
    [else (ret 'symbol #f)]))
