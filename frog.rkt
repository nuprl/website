#lang frog/config

; Minimal frog config to allow `raco frog -p` in this directory

(define/contract (init) (-> any) (void))

(define/contract (enhance-body xs)
  (-> (listof xexpr/c) (listof xexpr/c))
  xs)

(define/contract (clean)
  (-> any)
  (void))
