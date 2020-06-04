#lang racket/base
(require "util.rkt")

(provide asm-code)

(define asm-code
  (tts
   "subi  (imm 16), %sfp"
   "mov   %ac0, %rcx"
   "mov   (disp 8 %sfp), %ac0"
   "cmpi  (imm 6), %rcx"))
