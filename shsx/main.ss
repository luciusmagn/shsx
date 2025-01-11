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

(def example-page2
  (shsx
   (html:
    (head:
     (title: "My SHSX Demo")
     (meta: charset: "utf-8")
     (link: rel: "stylesheet" href: "style.css"))
    (body:
     (div: class: "gallery"
           (img: src: "cat1.jpg" alt: "Cat 1")
           (br:)
           (img: src: "cat2.jpg" alt: "Cat 2"))
     (hr:)
     (form:
      (input: type: "text" placeholder: "Your name")
      (br:)
      (input: type: "submit" value: "Send"))))))


(def admin? #f)  ; try changing these
(def email-verified? #t)
(def has-notifications? #t)
(def notification-count 7)
(def maintenance-mode? #t)
(def premium-user? #t)

(def example-page3
  (shsx
   (div: class: "content"
         (h1: "Welcome")
         ,(@if admin?
            (div: class: "admin-panel"
                  (h2: "Admin Controls")
                  (button: "Delete Stuff"))
            (p: "Please log in as admin"))
         (footer: "Always visible"))))


(def logged-in? #t)
(def posts '("Post 1" "Post 2"))
(def dark-mode? #t)

(def example-page4
  (shsx
   (div: class: "app"
         ;; Nested @if
         ,(@if logged-in?
            (div: class: "profile"
                  (h2: "Welcome back!")
                  ,(@if admin?
                     (span: class: "badge" "Admin")
                     (span: class: "badge" "User")))
            (div: class: "login-prompt"
                  (h2: "Please log in")))

         ;; @if with empty branch
         ,(@if (null? posts)
            (p: "No posts yet!")
            (div: class: "posts"
                  (h3: "Recent Posts")
                  (p: ,(car posts))))

         ;; @if affecting attributes
         (div: class: ,(string-append
                        "theme-"
                        (if dark-mode? "dark" "light"))
               (p: "Theme-aware content"))

         ;; @if with self-closing tags
         ,(@if logged-in?
            (img: src: "avatar.jpg" alt: "Your avatar")
            (img: src: "default-avatar.jpg" alt: "Default avatar"))

         ;; Multiple conditions in sequence
         ,(@if admin?
            (button: class: "danger" "Delete")
            (button: class: "primary" "Edit"))
         ,(@if logged-in?
            (button: "Logout")
            (button: "Login")))))

(def simple-test
  (shsx
   (div: class: "test-container"
         ;; Basic @if
         ,(@if #t
            ,(@if #t
               (p: "Welcome back!")
               (p: "NS!"))
            (p: "Please log in")))))

(def begin-test
  (shsx
   (div: class: "test-container"
         ;; Basic @begin
         ,(@begin
            (p: "First paragraph")
            (p: "Second paragraph"))

         ;; @begin with @if
         ,(@if #t
            ,(@begin
               (h1: "Title")
               (p: "Content"))
            (p: "Not shown")))))

(def when-unless-test
  (shsx
   (div: class: "test-container"
         ;; Basic @begin
         ,(@when #f
            (p: "First paragraph")
            (p: "Second paragraph"))

         ;; @begin with @if
         ,(@if #t
            ,(@unless #f
               (h1: "Title")
               (p: "Content"))
            (p: "Not shown")))))

(def user-state
  (shsx
   (div: class: "dashboard"
         ,(@if logged-in?
            ,(@begin
               (nav: class: "user-nav"
                     ,(@when admin?
                        (a: href: "/admin" "Admin Panel"))
                     (a: href: "/profile" "Profile")
                     (a: href: "/logout" "Logout"))
               ,(@unless email-verified?
                  (div: class: "warning"
                        (p: "Please verify your email")
                        (button: "Resend verification"))))
            ,(@begin
               (h2: "Welcome Guest")
               (a: href: "/login" "Log In")))
         (main: class: "content"
                ,(@when has-notifications?
                   (div: class: "notifications"
                         ,(@if (> notification-count 5)
                            (p: class: "urgent" "You have many notifications!")
                            (p: "You have new notifications"))))
                ,(@unless maintenance-mode?
                   (div: class: "features"
                         (h3: "Available Features")
                         ,(@when premium-user?
                            (p: "Premium features enabled"))))))))

;; Interactive REPL
(def (main . args)
  (displayln (render-html example-page))
  (displayln (render-html example-page2))
  (displayln (render-html simple-test))
  (displayln (render-html begin-test))
  (displayln (render-html when-unless-test))
  (displayln (render-html example-page3))
  (displayln (render-html example-page4)))
