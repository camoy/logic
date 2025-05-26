#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide dfa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/list
		     syntax/parse)
         (prefix-in ^ automata/dfa)
         automata/machine
         syntax-spec-v3
         "match.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar

(syntax-spec
 (host-interface/expression
  (dfa start:id
       (end:id ...)
       [from:id pat:match-pat to:id] ...)
  #:binding (scope (import pat) ...)
  (with-syntax ([(([f p0 t0] [_ p t] ...) ...)
                 (group-by (λ (x) (first (syntax->datum x)))
                           (syntax-e #'([from pat to] ...)))])
    #'(^dfa start
            (end ...)
            [f ([(? (compile-pat p0)) t0]
                [(? (compile-pat p)) t] ...)] ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler

(define-syntax-rule (compile-pat p)
  (λ (x) (match x [p #t] [_ #f])))
