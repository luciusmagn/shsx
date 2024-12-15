(library (shsx element)
  (export make-element element?
          element-type element-props element-children
          render-to-string)
  (import (rnrs))

  (define-record-type element
    (fields type props children))

  ;; TODO: Add rendering logic
  (define (render-to-string element)
    (error 'render-to-string "Not implemented yet")))
