#lang racket

;; Defines common publication data used in multiple files

(provide (struct-out publication))

;; string string string number (maybe string) -> elem?
(define-struct publication (title authors venue year link) #:prefab)
