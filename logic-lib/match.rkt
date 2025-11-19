#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-space match-space (all-defined-out))
         (for-syntax match-var
                     match-pat)
	 define-match-expander
	 match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
		     syntax/parse
                     syntax/parse/class/struct-id)
         syntax-spec-v3
	 racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar

(syntax-spec
 (binding-class match-var
   #:description "match pattern variable"
   #:reference-compiler immutable-reference-compiler)

 (extension-class match-macro
   #:binding-space match-space)

 (nonterminal/exporting match-pat
  #:description "match pattern"
  #:bind-literal-set match-pat-literals
  #:allow-extension match-macro
  #:binding-space match-space

  _
  x:match-var
  #:binding (export x)
  (s:struct-id p:match-pat ...)
  #:binding [(re-export p) ...]
  (and2 p1:match-pat p2:match-pat)
  #:binding [(re-export p1) (re-export p2)]
  (? pred:racket-expr)
  (app proc:racket-expr p:match-pat)
  #:binding (re-export p)
  (not p:match-pat)
  #:binding (scope (import p)))

 (nonterminal match-clause
   #:description "match clause"
   [p:match-pat body:racket-expr ...+]
   #:binding (scope (import p) body ...))

 (host-interface/expression
  (match target:racket-expr c:match-clause ...)
  #'(let ([target-pv target])
        (do-match-clauses target-pv c ...))))

(begin-for-syntax
  (define match-space-intro
    (make-interned-syntax-introducer 'match-space)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper macros

(define-syntax do-match-clauses
  (syntax-parser
    [(_ target:id)
     #'(error 'match "no matching clause for ~a" target)]
    [(_ target:id [p body ...+] c ...)
     #'(do-match target p (begin body ...) (do-match-clauses target c ...))]))

(define-syntax do-match
  (syntax-parser
    [(_ target:id pat on-success on-fail)
     (syntax-parse #'pat
       #:datum-literals (_ and2 ? var app not)
       [_
        #'on-success]
       [x:id
        #'(let ([x target]) on-success)]
       [(var x:id)
        #'(let ([x target]) on-success)]
       [(s:struct-id p ...)
        #'(if (s.predicate-id target)
              (do-match-struct target (s.accessor-id ...) (p ...) on-success on-fail)
              on-fail)]
       [(and2 p1 p2)
        #'(do-match target p1 (do-match target p2 on-success on-fail) on-fail)]
       [(? pred)
        #'(if (pred target) on-success on-fail)]
       [(app proc p)
        #'(let ([new-target (proc target)])
            (do-match new-target p on-success on-fail))]
       [(not p)
        #'(do-match target p on-fail on-success)])]))

(define-syntax do-match-struct
  (syntax-parser
    [(_ target () () on-success on-fail) #'on-success]
    [(_ target (a0 a ...) (p0 p ...) on-success on-fail)
     #'(let ([target-field (a0 target)])
         (do-match target-field
                   p0
                   (do-match-struct target (a ...) (p ...) on-success on-fail)
                   on-fail))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; match expanders

(define-syntax define-match-expander
  (syntax-parser
    [(_ name:id trans:expr)
     #:with name* (match-space-intro #'name 'add)
     #`(define-syntax name* (match-macro trans))]))

(define-match-expander and
  (syntax-parser
    [(_) #'_]
    [(_ p0 p ...)
     #'(and2 p0 (and p ...))]))

(define-match-expander equal?
  (syntax-parser
    [(_ val)
     #'(? (lambda (x) (equal? x val)))]))

(define-match-expander cons
  (syntax-parser
    [(_ car-pat cdr-pat)
     #'(and (? cons?)
            (app car car-pat)
            (app cdr cdr-pat))]))

(define-match-expander list
  (syntax-parser
    [(_) #''()]
    [(_ p0 p ...)
     #'(cons p0 (list p ...))]))

(define-match-expander quote
  (syntax-parser
    [(_ lit)
     #'(equal? 'lit)]))

(define-match-expander quasiquote
  (syntax-parser
    [(_ x:id) #''x]
    [(_ ((~datum unquote) p)) #'p]
    [(_ (qp ...)) #'(list (quasiquote qp) ...)]))
