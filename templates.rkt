#lang racket
(require web-server/templates)

(provide (all-defined-out))

(define (header title) (include-template "templates/header.html"))
(define footer (include-template "templates/footer.html"))
(define (navbar current-page) (include-template "templates/nav.html"))

(define (subpage-title title)
  (include-template "templates/subpage-title.html"))
