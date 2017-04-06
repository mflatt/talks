#lang racket
(require sobj/lang/parser)

(define input "{ main = 5; }")

(define port (open-input-string input))

(define stx (parse 'string port))
(syntax->datum stx)

;(require parser-tools/lex)
;(position-token-token (lex port))
;(position-token-token (lex port))
