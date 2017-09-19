#lang racket/base
(require
  rackunit
  redex/reduction-semantics
  (only-in syntax/macro-testing convert-compile-time-error))

(define-language nat
  [N ::= Zero (Plus1 N)])

(check-true
  (redex-match? nat N (term Zero)))

(check-true
  (redex-match? nat N (term (Plus1 Zero))))

(check-true
  (redex-match? nat N_0 (term Zero)))

(check-false
  (redex-match? nat (Plus1 N_0) (term Zero)))

(check-true
  (redex-match? nat (Plus1 N_0) (term (Plus1 (Plus1 Zero)))))

(check-pred pair?
  (redex-match nat N_0 (term Zero)))

(check-false
  (redex-match nat (Plus1 N_0) (term Zero)))

(check-pred pair?
  (redex-match nat (Plus1 N_0) (term (Plus1 (Plus1 Zero)))))

(define-language trees
  [binary-tree ::= Leaf
                   (Node binary-tree binary-tree)])

(check-pred pair?
  (redex-match trees
    (Node binary-tree_left binary-tree_right)
    (term (Node Leaf (Node Leaf Leaf)))))

(check-true
  (redex-match? nat _ (term Zero)))

(check-true
  (redex-match? nat _ (term (A (B C)))))

(check-true
  (redex-match? nat (Plus1 _) (term (Plus1 9))))

(check-pred pair?
  (redex-match trees
    (Node _ _)
    (term (Node Leaf (Node Leaf Leaf)))))

(check-true
  (redex-match? nat (N_0 ...) (term ())))

(check-true
  (redex-match? nat (N_0 ...) (term (Zero))))

(check-true
  (redex-match? nat (N_0 ...) (term (Zero Zero Zero))))

(check-pred pair?
  (redex-match nat (N_0 ...) (term (Zero Zero Zero))))

(check-pred pair?
  (redex-match nat (N_0 N_1 ... N_2) (term (Zero Zero))))

(check-exn exn:fail:syntax?
  (λ () (convert-compile-time-error (redex-match? nat ... (term Zero)))))

(check-exn exn:fail:syntax?
  (λ () (convert-compile-time-error (redex-match? nat (...) (term Zero)))))

(define-judgment-form nat
  #:mode (N= I I)
  #:contract (N= N N)
  [
   --- Zero=
   (N= Zero Zero)]
  [
   (where (Plus1 N_0--) N_0)
   (where (Plus1 N_1--) N_1)
   (N= N_0-- N_1--)
   ---
   (N= N_0 N_1)])

(check-true (judgment-holds (N= Zero Zero)))
(check-true (judgment-holds (N= (Plus1 (Plus1 Zero)) (Plus1 (Plus1 Zero)))))
(check-false (judgment-holds (N= (Plus1 Zero) (Plus1 (Plus1 Zero)))))

(define-metafunction nat
  N=? : N N -> boolean
  [(N=? Zero Zero)
   #true]
  [(N=? N_0 N_1)
   (N=? N_0-- N_1--)
   (where (Plus1 N_0--) N_0)
   (where (Plus1 N_1--) N_1)]
  [(N=? N_0 N_1)
   #false])

(check-true (term (N=? Zero Zero)))
(check-true (term (N=? (Plus1 (Plus1 Zero)) (Plus1 (Plus1 Zero)))))
(check-false (term (N=? (Plus1 Zero) (Plus1 (Plus1 Zero)))))

(define-language Arith
  (e ::= integer (e + e))
  (τ ::= Int))

(define-judgment-form Arith
  #:mode (infer-type I O)
  #:contract (infer-type e τ)
  [
   --- T-Int
   (infer-type e_0 Int)])

(define-language SomeTypes
  (τ ::= (→ τ τ) Integer))

(check-exn exn:fail:syntax?
  (λ () (convert-compile-time-error
          (define-judgment-form SomeTypes
            #:mode (<: I I)
            #:contract (<: τ τ)
            [
             (<: τ_0 τ_1)
             (<: τ_1 τ_2)
             --- S-Trans
             (<: τ_0 τ_2)]
            [
             --- S-Refl
             (<: τ_0 τ_0)]
            [
             (<: τ_dom-1 τ_dom-0)
             (<: τ_cod-0 τ_cod-1)
             --- S-Arrow
             (<: (→ τ_dom-0 τ_cod-0) (→ τ_dom-1 τ_cod-1))]))))

(define-language Λ
  [e ::= (e e) x (λ x e)]
  [x ::= variable-not-otherwise-mentioned]
  #:binding-forms
  (λ x_0 e_0 #:refers-to x_0))

(check-true
  (alpha-equivalent? Λ
    (term (λ x x))
    (term (λ y y))))

(define-metafunction Λ
  test-substitute : e -> e
  [(test-substitute (λ x_0 e_0))
   (substitute e_0 x_0 y)])

(check-equal?
  (term (test-substitute (λ z (z z))))
  (term (y y)))
