;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/format
        :std/misc/string
        :std/srfi/13
        :std/srfi/113)
(export #t)

(define sanitize-pairs
  '(("&"  . "&amp;")
    ("<"  . "&lt;")
    (">"  . "&gt;")
    ("\"" . "&quot;")
    ("'"  . "&#39;")))

(define (string-replace-str source old new)
  (let loop ((pos 0)
             (result ""))
    (let ((idx (string-contains source old pos)))
      (if idx
        (loop (+ idx (string-length old))
              (string-append result
                             (substring source pos idx)
                             new))
        (string-append result
                       (substring source pos (string-length source)))))))


(define (sanitize str)
  (foldl (lambda (pair acc)
           (string-replace-str acc (car pair) (cdr pair)))
         str
         sanitize-pairs))

(define self-closing-tags
  '(area: base: br: col: embed: hr: img:
    input: link: meta: param: source: track: wbr:))

(define (keyword->tagname tag)
  (string-trim-suffix ":" (symbol->string (keyword->symbol tag))))

(define (render-html expr)
  (match expr
    ((? string?) expr)
    ((? symbol?) (symbol->string expr))
    ((cons tag attrs-and-children)
     (let* ((tag-name (keyword->tagname tag))
            (attrs+children (parse-attrs+children attrs-and-children)))
       (if (memq tag self-closing-tags)
         (format "<~a~a>"
                 tag-name
                 (render-attrs (car attrs+children)))
         (if (eq? tag fragment:)
           (format "~a"
                   (render-children (cdr attrs+children)))
           (format "<~a~a>~a</~a>"
                   tag-name
                   (render-attrs (car attrs+children))
                   (render-children (cdr attrs+children))
                   tag-name)))))))

(define (parse-attrs+children lst)
  (let loop ((rest lst) (attrs '()))
    (match rest
      ((cons (? keyword? k) (cons v more))
       (loop more (cons (cons k v) attrs)))
      (children
       (cons (reverse attrs) children)))))

(define (render-attrs attrs)
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

(define (render-children children)
  (string-join (map render-html children) ""))

;; quasiqoute lmao
(defsyntax (shsx stx)
  (syntax-case stx ()
    ((shsx expr) #'`expr)))

(defsyntax (@if stx)
  (syntax-case stx ()
    ((@if condition true false) #'(if condition
                                    `true
                                    `false))))

(defsyntax (@begin stx)
  (syntax-case stx ()
    ((@begin children ...)
     #'(shsx (fragment: children ...)))))

(defsyntax (@when stx)
  (syntax-case stx ()
    ((@when condition children ...)
     #'(@if condition
        ,(@begin children ...)
        (fragment:)))))

(defsyntax (@unless stx)
  (syntax-case stx ()
    ((@unless condition children ...)
     #'(@when (not condition) children ...))))
