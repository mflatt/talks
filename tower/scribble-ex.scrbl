#lang scribble/manual
@(require (for-label racket/base))

@title{How to Build a Language}

To build a language, start with PLT Racket, and then use macros.
You can extend Racket simply by using syntactic forms like 
@racket[define-syntax-rule], or you can create an entirely
new language starting with @hash-lang[].

@section{Language Extension}

Here is an example of using @racket[define-syntax-rule]:

@racketblock[
 (define-syntax-rule (define-thing id 
                       [cmd response ...] ...)
   (begin
     (define id (make-thing 'id #f (actions [cmd response ...] ...)))
     (record-object! 'id id)))
]

@section{Language Construction}

To implement a @hash-lang[] reader, it's best to start with the
@racketmodname[syntax/module-reader] language:

@racketmod[
syntax/module-reader
]
