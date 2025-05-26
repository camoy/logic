#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-space re-space (all-defined-out))
         define-re
         define-re-rule
         re)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
		     syntax/parse)
         (prefix-in ^ racket/match)
         automata/machine
         syntax-spec-v3
         "match.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar

(syntax-spec
 (binding-class re-def
   #:description "RE definition")

 (extension-class re-macro
   #:binding-space re-space)

 (nonterminal re-expr
  #:description "RE expression"
  #:bind-literal-set re-expr-literals
  #:allow-extension re-macro
  #:binding-space re-space

  x:re-def
  (complement e:re-expr)
  (seq e:re-expr ...)
  (union e:re-expr ...)
  (star e:re-expr)
  (epsilon)
  (nullset)
  pat:match-pat
  #:binding (scope (import pat)))

 (host-interface/expression
  (re e:re-expr)
  #'(compile-re e))

 (host-interface/definitions
  (define-re x:re-def e:re-expr)
  #:binding (export x)
  #'(define x (compile-re e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler

(define-syntax compile-re
  (syntax-parser
    #:datum-literals (complement seq union star epsilon nullset)
    [(_ (complement e)) #'(machine-complement (compile-re e))]
    ;; TODO: these only take in a finite number of arguments
    [(_ (seq e ...)) #'(machine-seq (compile-re e) ...)]
    [(_ (union e ...)) #'(machine-union (compile-re e) ...)]
    [(_ (star e)) #'(machine-star (compile-re e))]
    [(_ (epsilon)) #'machine-epsilon]
    [(_ (nullset)) #'machine-null]
    [(_ x:id) #'x]
    [(_ p)
     #'(machine 'p
                (λ (x)
                  (match x
                    [p machine-epsilon]
                    [_ machine-null])))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expander macros

(begin-for-syntax
  (define re-space-intro
    (make-interned-syntax-introducer 're-space)))

(define-syntax define-re-expander
  (syntax-parser
    [(_ name:id trans:expr)
     #:with name* (re-space-intro #'name 'add)
     #'(define-syntax name* (re-macro trans))]))

(define-syntax define-re-rule
  (syntax-parser
    [(_ h:id b:expr)
     #'(define-re-expander h
         (syntax-parser
           [_ (syntax/loc this-syntax b)]))]
    [(_ (h:id a:expr ...) b:expr)
     #'(define-re-expander h
         (syntax-parser
           [(_ a ...) (syntax/loc this-syntax b)]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expanders

(define-re-rule (plus e)
  (seq e (star e)))

(define-re-rule (opt e)
  (union (epsilon) e))
