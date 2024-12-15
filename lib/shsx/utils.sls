(library (shsx utils)
  (export escape-html-attribute
          escape-html-text-content)
  (import (rnrs))

  (define char-replacements
    '((#\& . "&amp;")
      (#\< . "&lt;")
      (#\> . "&gt;")
      (#\" . "&quot;")
      (#\' . "&#x27;")
      (#\/ . "&#x2F;")
      (#\` . "&grave;")
      (#\= . "&#x3D;")))

  (define (escape-char c replacements)
    (cond
     [(assq c replacements) => cdr]
     [else (string c)]))

  (define (escape-string str replacements)
    (if (string? str)
        (apply string-append
               (map (lambda (c)
                      (escape-char c replacements))
                    (string->list str)))
        str))

  (define (escape-html-text-content str)
    (escape-string str char-replacements))

  (define (escape-html-attribute str)
    (escape-string str
                   (list (cons #\" "&quot;")))))
