#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-space pltl-space (all-defined-out))
         define-pltl
         define-pltl-rule
         pltl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
		     syntax/parse
                     syntax/parse/class/struct-id)
         automata/machine
         racket/match
         racket/set
         syntax-spec-v3
         "private/pltl/formula.rkt"
         "private/pltl/monitor.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar

(syntax-spec
 (binding-class pltl-var
   #:description "PLTL pattern variable")

 (binding-class pltl-def
   #:description "PLTL definition")

 (extension-class pltl-macro
   #:binding-space pltl-space)

 (nonterminal pltl-pat
  #:description "PLTL pattern"
  #:bind-literal-set pltl-pat-literals
  #:allow-extension pltl-macro
  #:binding-space pltl-space

  (~> x:id #'(var x))
  (var x:pltl-var)
  (and p:pltl-pat q:pltl-pat)
  (? e:racket-expr)
  (app e:racket-expr p:pltl-pat)
  (x:struct-id p:pltl-pat ...))

 (nonterminal pltl-formula
  #:description "PLTL formula"
  #:bind-literal-set pltl-formula-literals
  #:allow-extension pltl-macro
  #:binding-space pltl-space

  x:pltl-def
  (neg f:pltl-formula)
  (disj f:pltl-formula g:pltl-formula)
  (conj f:pltl-formula g:pltl-formula)
  (exists (x:pltl-var ...) f:pltl-formula)
  #:binding (scope (bind x) ... f)
  (previous f:pltl-formula)
  (since f:pltl-formula g:pltl-formula)
  p:pltl-pat)

 (host-interface/expression
  (pltl f:pltl-formula)
  (check-monitorability! #'f)
  #'(let ([ϕ (compile-pltl-formula f)] [state #f])
      (machine state (pltl-machine-next ϕ state))))

 (host-interface/definitions
  (define-pltl x:pltl-def f:pltl-formula)
  #:binding (export x)
  (check-monitorability! #'f)
  #'(define x (compile-pltl-formula f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; static semantics

(begin-for-syntax
  (define (monitorable-error! stx)
    (raise-syntax-error #f "not monitorable" stx))

  (define (fvs stx)
    (free-identifiers stx #:allow-host? #t))

  (define (fvs=? x y)
    (and (fvs-subset? x y)
         (fvs-subset? y x)))

  (define (fvs-subset? x y)
    (null? (remove* (fvs y) (fvs x) compiled-identifier=?)))

  (define (check-monitorability! stx)
    (syntax-parse stx
      #:datum-literals (neg disj conj exists previous since)
      [(disj f g)
       (unless (fvs=? #'f #'g)
         (monitorable-error! this-syntax))
       (check-monitorability! #'f)
       (check-monitorability! #'g)]
      [(~or (conj g (neg f))
            (conj (neg f) g)
            (since (neg f) g)
            (since f g))
       (unless (fvs-subset? (fvs #'f) (fvs #'g))
         (monitorable-error! this-syntax))
       (check-monitorability! #'f)
       (check-monitorability! #'g)]
      [(conj f g)
       (check-monitorability! #'f)
       (check-monitorability! #'g)]
      [(~or (exists (x:id ...) f)
            (previous f))
       (check-monitorability! #'f)]
      [(neg _)
       (monitorable-error! this-syntax)]
      [p
       #'(void)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler

(struct pltl-machine (formula state accepts?))

(define-syntax compile-pltl-formula
  (syntax-parser
    #:datum-literals (neg disj conj exists previous since)
    [(_ (neg f))
     #'(neg-fml (compile-pltl-formula f))]
    [(_ (disj f g))
     #'(disj-fml (compile-pltl-formula f) (compile-pltl-formula g))]
    [(_ (conj f g))
     #'(conj-fml (compile-pltl-formula f) (compile-pltl-formula g))]
    [(_ (exists (x ...) f))
     #'(exists-fml (set 'x ...) (compile-pltl-formula f))]
    [(_ (previous f))
     #'(previous-fml (compile-pltl-formula f))]
    [(_ (since f g))
     #'(since-fml (compile-pltl-formula f) (compile-pltl-formula g))]
    [(_ x:id)
     #'x]
    [(_ p)
     #'(compile-pltl-pat p)]))

(define-syntax compile-pltl-pat
  (syntax-parser
    [(_ p)
     #:with (bv ...) (free-identifiers #'p #:allow-host? #t)
     #'(pat-fml (set 'bv ...)
                (λ (x)
                  (match x
                    [p (hash (~@ 'bv bv) ...)]
                    [_ #f]))
                'p)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; runtime

(define ((pltl-machine-next ϕ state) evt)
  (define-values (env state*)
    (pltl-environments ϕ evt state))
  (define make
    (if (set-empty? env)
        machine
        machine-accepting))
  (make state* (pltl-machine-next ϕ state*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expander macros

(begin-for-syntax
  (define pltl-space-intro
    (make-interned-syntax-introducer 'pltl-space)))

(define-syntax define-pltl-expander
  (syntax-parser
    [(_ name:id trans:expr)
     #:with name* (pltl-space-intro #'name 'add)
     #'(define-syntax name* (pltl-macro trans))]))

(define-syntax define-pltl-rule
  (syntax-parser
    [(_ h:id b:expr)
     #'(define-pltl-expander h
         (syntax-parser
           [_ (syntax/loc this-syntax b)]))]
    [(_ (h:id a:expr ...) b:expr)
     #'(define-pltl-expander h
         (syntax-parser
           [(_ a ...) (syntax/loc this-syntax b)]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expanders

(define-pltl-rule (¬ x)
  (neg x))

(define-pltl-expander ∨
  (syntax-parser
    [(_ x) #'x]
    [(_ x y ...)
     #'(disj x (∨ y ...))]))

(define-pltl-expander ∧
  (syntax-parser
    [(_ x) #'x]
    [(_ x y ...)
     #'(conj x (∧ y ...))]))

(define-pltl-rule (∃ (x:id ...) f)
  (exists (x ...) f))

(define-pltl-rule (● x)
  (previous x))

(define-pltl-rule (S x y)
  (since x y))

(define-pltl-rule (♦︎ ϕ)
  (S ⊤ ϕ))

(define-pltl-rule ⊤
  (? (λ _ #true)))
