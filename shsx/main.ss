;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/format
        :std/srfi/13
        :std/misc/ports
        :std/test
        "./lib")
(export main)

(include "../manifest.ss")

;; Test state variables
(define admin? #f)
(define email-verified? #t)
(define has-notifications? #t)
(define notification-count 7)
(define maintenance-mode? #t)
(define premium-user? #t)
(define logged-in? #t)
(define posts '("Post 1" "Post 2"))
(define dark-mode? #t)

;; Test fixtures
(define example-page
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

(define self-closing-test
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

(define if-test
  (shsx
   (div: class: "content"
         (h1: "Welcome")
         ,(@if admin?
            (div: class: "admin-panel"
                  (h2: "Admin Controls")
                  (button: "Delete Stuff"))
            (p: "Please log in as admin"))
         (footer: "Always visible"))))

(define nested-if-test
  (shsx
   (div: class: "app"
         ,(@if logged-in?
            (div: class: "profile"
                  (h2: "Welcome back!")
                  ,(@if admin?
                     (span: class: "badge" "Admin")
                     (span: class: "badge" "User")))
            (div: class: "login-prompt"
                  (h2: "Please log in")))
         ,(@if (null? posts)
            (p: "No posts yet!")
            (div: class: "posts"
                  (h3: "Recent Posts")
                  (p: ,(car posts))))
         (div: class: ,(string-append
                        "theme-"
                        (if dark-mode? "dark" "light"))
               (p: "Theme-aware content"))
         ,(@if logged-in?
            (img: src: "avatar.jpg" alt: "Your avatar")
            (img: src: "default-avatar.jpg" alt: "Default avatar"))
         ,(@if admin?
            (button: class: "danger" "Delete")
            (button: class: "primary" "Edit"))
         ,(@if logged-in?
            (button: "Logout")
            (button: "Login")))))

(define simple-if-test
  (shsx
   (div: class: "test-container"
         ,(@if #t
            ,(@if #t
               (p: "Welcome back!")
               (p: "NS!"))
            (p: "Please log in")))))

(define begin-test
  (shsx
   (div: class: "test-container"
         ,(@begin
            (p: "First paragraph")
            (p: "Second paragraph"))
         ,(@if #t
            ,(@begin
               (h1: "Title")
               (p: "Content"))
            (p: "Not shown")))))

(define when-unless-test
  (shsx
   (div: class: "test-container"
         ,(@when #f
            (p: "First paragraph")
            (p: "Second paragraph"))
         ,(@if #t
            ,(@unless #f
               (h1: "Title")
               (p: "Content"))
            (p: "Not shown")))))

(define complex-test
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

(define (main . args)
  (displayln "\nSHSX Test Suite")
  (displayln "==============")

  (run-test-suite!
   (test-suite "Basic HTML Generation"
     (test-case "Simple page structure"
       (check (string? (render-html example-page)) => #t))

     (test-case "Self-closing tags"
       (check (string? (render-html self-closing-test)) => #t))))

  (run-test-suite!
   (test-suite "Control Flow Features"
     (test-case "Simple @if"
       (check (string? (render-html simple-if-test)) => #t))

     (test-case "@begin directive"
       (check (string? (render-html begin-test)) => #t))

     (test-case "@when/@unless directives"
       (check (string? (render-html when-unless-test)) => #t))))

  (run-test-suite!
   (test-suite "Complex Compositions"
     (test-case "Basic @if test"
       (check (string? (render-html if-test)) => #t))

     (test-case "Nested @if test"
       (check (string? (render-html nested-if-test)) => #t))

     (test-case "Complex control flow"
       (check (string? (render-html complex-test)) => #t)))))
