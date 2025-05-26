#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-space sl-space (all-defined-out))
         sl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/set
                     syntax/parse
                     "private/sl/cycles.rkt")
         automata/machine
         racket/function
         syntax-spec-v3
         "private/sl/solver.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar

(define-syntax sl
  (syntax-parser
    [(_ (x:id ...) [y:id e:expr] ...)
     #'(compile-sl (x ...) [y e] ...)]))

#;(syntax-spec
 (host-interface/expression
  (sl (x:racket-var ...) [y:racket-var e:racket-expr] ...)
  #:binding (scope (bind x) ... (bind y) ... e ...)
  #'(compile-sl (x ...) [y e] ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler

(begin-for-syntax
  (define current-id-set (make-parameter #f))

  (define (make-var x lhs)
    (syntax-parser
      [x:id #'(x 0 #f)]
      [(x:id n:number e:expr)
       (set-add! (current-id-set)
                 (list (syntax-e #'x)
                       (syntax-e #'n)
                       (syntax-e lhs)))
       #'(solve-for-var 'x n e)]))

  (define (check-cyclicity! ids stx)
    (define graph
      (for/fold ([graph (hash)])
                ([edge (in-mutable-set ids)])
        (match-define (list to weight from) edge)
        (hash-update graph from
                     (λ (edges) (set-add edges (list to weight)))
                     (set (list to weight)))))
    (define cycle-path-if-exists (check-cycles graph))
    (when cycle-path-if-exists
      (define fmt "found cyclic dependency in stream equations: ~a")
      (define eqns (graph-path->string (first (last cycle-path-if-exists)) cycle-path-if-exists))
      (raise-syntax-error #f (format fmt eqns))))
  )

(define-syntax (compile-sl stx)
  (parameterize ([current-id-set (mutable-set)])
    (syntax-parse stx
      [(_ (evt:id ...) [lhs:id rhs:expr] ...+)
       #:with root-var (first (syntax-e #'(lhs ...)))
       #:do [(define eqns #'(list (make-equation lhs rhs (evt ... lhs ...)) ...))]
       #:with ^eqns (local-expand eqns 'expression '())
       #:with (dep ...) (set->list (current-id-set))
       (check-cyclicity! (current-id-set) stx)
       #'(sl-machine
          'root-var
          (monitor (hash) (hash) '(dep ...) ^eqns '(evt ...) 0))])))

(define-syntax make-equation
  (syntax-parser
    [(_ lhs:id rhs:expr (var:id ...))
     #:do [(define eq-rhs
             #'(let-syntax ([var (make-var #'var #'lhs)]...)
                 (thunk rhs)))
           (define-values (^eq-rhs deps)
             (parameterize ([current-id-set (mutable-set)])
               (values (local-expand eq-rhs 'expression '())
                       (current-id-set))))
           (set-union! (current-id-set) deps)]
     #`(eqn 'lhs '#,(map first (set->list deps)) #,^eq-rhs)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; runtime (machine)

(define (sl-machine root-var mon)
  (machine-accepting mon (sl-machine-next root-var mon)))

(define ((sl-machine-next root-var mon) evt)
  (define mon* (monitor-update mon evt))
  (define n (monitor-index mon))
  (define make
    (if (hash-ref (monitor-R mon*) (var root-var n))
        machine-accepting
        machine))
  (make mon* (sl-machine-next root-var mon*)))
