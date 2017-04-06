#lang slideshow
(require slideshow/code
         (prefix-in s: scribble/core)
         racket/draw)

(provide codeblock->pict)

(define (codeblock->pict block)
  (define (color-text name what)
    (define (float-part v)
      (inexact->exact (round (* 255 v))))

    (define-syntax-rule (define-color name r g b)
      (define name (make-object color%
                                (float-part r)
                                (float-part g)
                                (float-part b))))
    ;; \definecolor{PaleBlue}{rgb}{0.90,0.90,1.0}
    ;; \definecolor{LightGray}{rgb}{0.90,0.90,0.90}
    ;; \definecolor{CommentColor}{rgb}{0.76,0.45,0.12}
    ;; \definecolor{ParenColor}{rgb}{0.52,0.24,0.14}
    ;; \definecolor{IdentifierColor}{rgb}{0.15,0.15,0.50}
    ;; \definecolor{ResultColor}{rgb}{0.0,0.0,0.69}
    ;; \definecolor{ValueColor}{rgb}{0.13,0.55,0.13}
    ;; \definecolor{OutputColor}{rgb}{0.59,0.00,0.59}

    (define-color value-color 0.13 0.55 0.13)
    (define-color identifier-color 0.15 0.15 0.50)
    (define-color pale-blue 0.90 0.90 1.0)
    (define-color light-gray 0.90 0.90 0.90)
    (define-color comment-color 0.76 0.45 0.12)
    (define-color paren-color 0.52 0.24 0.14)
    (define-color result-color 0.0 0.0 0.69)
    (define-color output-color 0.59 0.0 0.59)
    (define-color black 0.0 0.0 0.0)

    ;; FIXME
    (define-color blue 0 0 1)

    ;; FIXME
    (define-color red 1 0 0)

    ;; FIXME
    (define-color light-grey 0.8 0.8 0.8)

    (define (get-color name)
      (match name
        ["RktMeta" black]
        ["RktPn" paren-color]
        ["RktPlain" black]
        ["RktKw" black]
        ["RktCmt" comment-color]
        ["RktPn" paren-color]
        ["RktInBG" paren-color]
        ["RktSym" identifier-color]
        ["RktVal" value-color]
        ["RktValLink" blue]
        ["RktModLink" #;blue identifier-color]
        ["RktRes" result-color]
        ["RktOut" output-color]
        ["RktMeta" identifier-color]
        ["RktMod" black]
        ["RktRdr" black]
        ["RktVarCol" identifier-color]

        ;; FIXME
        ;; \RktVarCol{\textsl{#1}}}
        ["RktVar" (get-color "RktVarCol")]

        ["RktErrCol" red]

        ;; FIXME:
        ;; {{\RktErrCol{\textrm{\textit{#1}}}}}
        ["RktErr" (get-color "RktErrCol")]

        ;; FIXME:
        ;; {\RktOpt}[1]{#1}
        ;;
        ["RktOpt" black]

        ;; FIXME: 
        ;;  }[1]{\incolorbox{LightGray}{\RktInBG{#1}}}
        ["RktIn" light-grey]

        ;; FIXME:
        ;;  }[1]{\colorbox{PaleBlue}{\hspace{-0.5ex}\RktInBG{#1}\hspace{-0.5ex}}}
        ["highlighted" pale-blue]
        [else (error 'color-text "unknown type type '~a'" name)]))

    (define out (text what '(bold . modern) (current-font-size)))
    (colorize out (get-color name)))

  (define (convert-element element)
    (match element
      [(struct s:element (style content))
       (match style
         ['hspace (apply tt content)]
         [(struct s:style (name properties))
          (color-text name (s:content->string content))])]))

  (define (convert-block block)
    (match block
      [(struct s:paragraph (style content))
       (for/fold ([start (blank)])
                 ([element content])
         (match element
           [(? string?) (hc-append start (t element))]
           [(struct s:element (style content))
            (hc-append start (convert-element element))]
           [else (error 'convert-block "don't know what to do with ~a" element)]))]))

  (define (convert-row row)
    (for/fold ([start (blank)])
              ([element row])
      (hc-append start (convert-block element))))
    
  ;; (pretty-print block)
  (match block
    [(struct s:nested-flow (style (list blocks ...)))
     (match (car blocks)
       [(struct s:table (style (list rows ...)))
        (for/fold ([start (blank)])
                  ([row rows])
          (vl-append (current-code-line-sep)
                     start
                     (convert-row row)))])]))
