#lang racket
(require web-server/templates)

(provide (all-defined-out))

(define (header title) (include-template "templates/header.html"))
(define footer (include-template "templates/footer.html"))
(define (navbar current-page) (include-template "templates/nav.html"))

(define (subpage-title title)
  (include-template "templates/subpage-title.html"))

;; website should include https or http
;;   should figure out a nice interface so it doesn't display the protocol on the page.
(define (person #:name name
                 #:title title
                 #:e-mail e-mail
                 #:website [website #f]
                 #:history history
                 #:bio     bio
                 #:img     img)
  (include-template "templates/person.html"))
