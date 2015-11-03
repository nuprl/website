#lang racket
(require web-server/templates)

(provide (all-defined-out))

(define header (include-template "templates/header.html"))
(define footer (include-template "templates/footer.html"))
(define navbar (include-template "templates/nav.html"))

(define (page-title title)
  (include-template "templates/page-title.html"))
