#lang scheme/gui

(require mrlib/switchable-button 
         mrlib/bitmap-label
         drscheme/tool
         scheme/system
         setup/xref)

(provide tool@)

(define-namespace-anchor anchor)

(define scribble-bm (make-object bitmap% 1 1))

(define tool@
  (unit
    (import drscheme:tool^)
    (export drscheme:tool-exports^)

    (define phase1 void)
    (define phase2 void)

    (define (make-new-unit-frame% super%)
      (class super%
        (inherit get-button-panel
                 get-definitions-text)
        (super-instantiate ())

        (define client-panel
          (new horizontal-pane% (parent (get-button-panel))))

        (define (make-render-button label mode suffix extra-cmdline)
          (new switchable-button%
               [label label]
               [bitmap scribble-bm]
               [parent client-panel]
               [callback
                (lambda (button)
                  (let* ([t (get-definitions-text)]
                         [fn (send t get-filename)])
                    (if fn
                        (begin
                          (send t save-file)
                          (parameterize ([current-namespace (make-base-namespace)]
                                         [current-command-line-arguments
                                          (list->vector 
                                           (append
                                            extra-cmdline
                                            (list mode (if (path? fn) (path->string fn) fn))))])
                            (namespace-attach-module (namespace-anchor->empty-namespace anchor) 'setup/xref)
                            (dynamic-require 'scribble/run #f)
                            (let-values ([(base name dir?) (split-path fn)])
                              (system (format "open ~a" (path-replace-suffix name suffix))))))
                        (message-box "Not Named" "Cannot render unsaved file"))))]))

        (inherit register-toolbar-button)
        (define pdf-button (make-render-button "PDF" "--pdf" #".pdf" null))
        (register-toolbar-button pdf-button)
        (define html-button (make-render-button "HTML" "--html" #".html" '("++xref-in" "setup/xref" "load-collections-xref")))
        (register-toolbar-button html-button)

        (send (get-button-panel) change-children
              (lambda (l) (cons client-panel (remq client-panel l))))))

    (drscheme:get/extend:extend-unit-frame make-new-unit-frame% #f)))
