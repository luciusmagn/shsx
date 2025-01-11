;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/format
        :std/misc/string)
(export #t)

(def self-closing-tags
  '(area: base: br: col: embed: hr: img:
    input: link: meta: param: source: track: wbr:))

(define (keyword->tagname tag)
  (string-trim-suffix ":" (symbol->string (keyword->symbol tag))))

(def (render-html expr)
  (match expr
    ((? string?) expr)
    ((? symbol?) (symbol->string expr))
    ((cons '@if (cons condition (cons then else)))
     (apply render-html (if condition then else)))
    ((cons tag attrs-and-children)
     (let* ((tag-name (keyword->tagname tag))
            (attrs+children (parse-attrs+children attrs-and-children)))
       (if (memq tag self-closing-tags)
         (format "<~a~a/>"
                 tag-name
                 (render-attrs (car attrs+children)))
         (format "<~a~a>~a</~a>"
                 tag-name
                 (render-attrs (car attrs+children))
                 (render-children (cdr attrs+children))
                 tag-name))))))

(def (parse-attrs+children lst)
  (let loop ((rest lst) (attrs '()))
    (match rest
      ((cons (? keyword? k) (cons v more))
       (loop more (cons (cons k v) attrs)))
      (children
       (cons (reverse attrs) children)))))

(def (render-attrs attrs)
  (if (null? attrs)
    ""
    (string-append
     ;; beginning of attrs, attrs, end of attrs
     " "
     (string-join
      (map (match <>
             ((cons k v)
              (format "~a=\"~a\"" (keyword->tagname k) v)))
           attrs)
      " "))))

(def (render-children children)
  (string-join (map render-html children) ""))

;; quasiqoute lmao
(defsyntax (shsx stx)
  (syntax-case stx ()
    ((shsx expr) #'`expr)))
