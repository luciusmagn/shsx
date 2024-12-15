#!chezscheme
(library-directories '("lib"))
(import (rnrs)
        (shsx))
(case-sensitive #t)
(extend-syntax #t)

(display
 (render-to-string
  (hsx
   (+div '(class "container")
         (+h1 '() "Hello, SHSX!")
         (+p '() "This is a test")
         (+ul '()
              (+li '() "Item 1")
              (+li '() "Item 2"))))))
