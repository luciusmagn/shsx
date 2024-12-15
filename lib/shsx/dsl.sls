(library (shsx dsl)
  (export hsx defcomp)
  (import (rnrs)
          (shsx element))

  (define-syntax shsx
    (syntax-rules ()
      [(_ (tag props ...))
       (make-element 'tag (list props ...) '())]))

  ;; TODO: Component system
  (define-syntax defcomp
    (syntax-rules ()
      [(_ name props body ...)
       (define name
         (lambda props body ...))])))
