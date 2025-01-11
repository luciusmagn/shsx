;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/format
        :std/srfi/13
        :std/misc/ports
        "./lib")
(export main)

(include "../manifest.ss")

;; Example of a more complex HTML structure
(def example-page
  (shsx
   (html:
    (head:
     (title: "My SHSX Demo")
     (meta: charset: "utf-8")
     (link: rel: "stylesheet" href: "style.css"))
    (body:
     (nav: class: "navbar"
           (ul:
            (li: (a: href: "/" "Home"))
            (li: (a: href: "/about" "About"))
            (li: (a: href: "/contact" "Contact"))))
     (main: class: "container"
            (h1: class: "title" "Welcome to SHSX")
            (div: class: "content"
                  (p: "This is a demonstration of SHSX, a simple HTML generator.")
                  (ul: class: "features"
                       (li: "Simple syntax")
                       (li: "No dependencies")
                       (li: "Just works™"))))
     (footer: class: "footer"
              (p: "© 2024 SHSX Project"))))))

;; Interactive REPL
(def (main . args)
  (displayln (render-html example-page)))
