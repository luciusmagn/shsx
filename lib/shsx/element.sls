(library (shsx element)
  (export make-element element?
          element-type element-props element-children
          render-to-string)
  (import (rnrs)
          (rnrs lists)
          (shsx utils))

  ;; Basic element record type
  (define-record-type element
    (fields type props children))

  ;; Self-closing tags list
  (define self-closing-tags
    '(area base br col embed hr img input
      link meta param source track wbr))

  ;; Non-escaping tags (for raw content)
  (define non-escaping-tags
    '(script style))

  (define (self-closing? type)
    (memq type self-closing-tags))

  (define (non-escaping? type)
    (memq type non-escaping-tags))

  (define (render-props props)
    (if (null? props)
        ""
        (let loop ((props props)
                   (result ""))
          (if (null? props)
              result
              (loop (cddr props)
                    (string-append result " "
                                   (symbol->string (car props))
                                   "=\""
                                   (escape-html-attribute
                                    (if (boolean? (cadr props))
                                        (symbol->string (car props))
                                        (cond
                                         [(number? (cadr props))
                                          (number->string (cadr props))]
                                         [else (cadr props)])))
                                   "\""))))))

  (define (render-to-string element)
    (cond
     [(string? element) (escape-html-text-content element)]
     [(element? element)
      (let* ((type (element-type element))
             (props (element-props element))
             (children (element-children element))
             (props-str (render-props props)))
        (cond
         [(eq? type '<>)
          (string-append-map render-to-string children)]
         [(self-closing? type)
          (string-append "<" (symbol->string type) props-str ">")]
         [else
          (string-append
           "<" (symbol->string type) props-str ">"
           (string-append-map render-to-string
                              (if (non-escaping? type)
                                  children
                                  (filter (lambda (x) x) children)))
           "</" (symbol->string type) ">")]))]))

  ;; Helper for string concatenation with mapping
  (define (string-append-map fn lst)
    (apply string-append
           (map fn lst))))
