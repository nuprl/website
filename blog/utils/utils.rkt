#lang racket/base

;; Utilities for Scribble-based blog posts

(require scribble/base
         scribble/manual
         scribble/racket
         (for-syntax racket/base))

(provide (rename-out [-racket racket]
                     [-racketmodname racketmodname])
         reftech
         ffitech
         define-dummy)

(define (reftech . x)
  (apply tech x #:doc '(lib "scribblings/reference/reference.scrbl")))
(define (ffitech . x)
  (apply tech x #:doc '(lib "scribblings/foreign/foreign.scrbl")))

; Dummy defs to disable underscore emphasis
(define-syntax (define-dummy stx)
  (syntax-case stx ()
    [(_ ?id)
     #`(define-syntax ?id
         (make-element-id-transformer
           (λ (x) #`(racketidfont #,(symbol->string (syntax-e x))))))]))

;; Wrap these for HTML output so that it's possible to inline code
(define-syntax (-racket stx)
  (syntax-case stx ()
    [(_ x) #'(elem #:style "RktWrap" (racket x))]))
(define-syntax (-racketmodname stx)
  (syntax-case stx ()
    [(_ x) #'(elem #:style "RktWrap" (racketmodname x))]))
